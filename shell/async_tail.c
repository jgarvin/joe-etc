#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sys/poll.h>
#include <sys/stat.h>
#include <errno.h>
#include <time.h>

/* async_tail is a wrapper that redirects your command to a log and
 * launches a tail that exits only when all output is printed. This
 * way you run `async_tail --log /tmp/foo -- cmd_a | cmd_b` the speed
 * of cmd_a is not throttled by cmd_b's consumption rate. */

#define BUFFER_SIZE 4096
#define LOG_DEBUG 0

struct timespec g_start, g_end;

static void execute_command(char *cmd[], char *log_path)
{
    // undo being set in parent
    if(signal(SIGCHLD, SIG_DFL) == SIG_ERR)
    {
        perror("signal");
        exit(EXIT_FAILURE);
    }

    int log_fd = open(log_path, O_WRONLY | O_CREAT | O_TRUNC, 0666);
    if (log_fd < 0)
    {
        perror("Failed to open log file");
        exit(EXIT_FAILURE);
    }

    if(dup2(log_fd, STDOUT_FILENO) < 0)
    {
        perror("dup2");
        exit(EXIT_FAILURE);
    }
    if(dup2(log_fd, STDERR_FILENO) < 0)
    {
        perror("dup2");
        exit(EXIT_FAILURE);
    }
    close(log_fd);

    execvp(cmd[0], cmd);
    perror("Failed to execute command");
    exit(EXIT_FAILURE);
}

volatile sig_atomic_t child_exited = 0;

static void sigchld_handler(int signum)
{
    // Set the flag to indicate the child has exited
    if(LOG_DEBUG) fprintf(stderr, "Child exited!\n");
    child_exited = 1;
}

static ssize_t forward_data(int log_fd, char* buffer, ssize_t* total_bytes_written)
{
    ssize_t bytes_read = 0;

    do {
        bytes_read = read(log_fd, buffer, BUFFER_SIZE);
    } while(bytes_read < 0 && errno != EINTR);

    if(bytes_read < 0)
    {
        perror("Failed to read from log file");
        exit(EXIT_FAILURE);
    }

    //fprintf(stderr, "bytes_read=%ld\n", bytes_read);
    ssize_t bytes_written_of_this_read = 0;
    while(bytes_written_of_this_read < bytes_read)
    {
        ssize_t bytes_written = write(STDOUT_FILENO, buffer, bytes_read);
        if (bytes_written < 0)
        {
            if(errno != EINTR)
            {
                perror("write");
                exit(EXIT_FAILURE);
            }
            continue;
        }
        bytes_written_of_this_read += bytes_written;
        *total_bytes_written += bytes_written;
    }

    return bytes_read;
}

int main(int argc, char *argv[])
{
    struct sigaction sa;
    sa.sa_handler = &sigchld_handler;
    sa.sa_flags = SA_NOCLDSTOP; // don't notify on stopped child, only on exit
    sigemptyset(&sa.sa_mask);
    if (sigaction(SIGCHLD, &sa, NULL) == -1) {
        perror("sigaction");
        exit(EXIT_FAILURE);
    }

    // unblock SIGCHLD in case we inherited it being blocked from parent process
    sigset_t sigset;
    sigemptyset(&sigset);
    sigaddset(&sigset, SIGCHLD);
    int result = sigprocmask(SIG_UNBLOCK, &sigset, NULL);
    if (result == -1) {
        perror("sigprocmask");
        exit(EXIT_FAILURE);
    }

    if(argc < 5 || strcmp(argv[1], "--log") != 0 || strcmp(argv[3], "--") != 0) {
        fprintf(stderr, "Usage: %s --log <log_path> -- <command>\n", argv[0]);
        return EXIT_FAILURE;
    }

    char *log_path = argv[2];
    char **cmd = &argv[4];

    int log_fd = open(log_path, O_RDONLY | O_NONBLOCK | O_CLOEXEC);
    if (log_fd < 0)
    {
        perror("Failed to open log file for reading");
        exit(EXIT_FAILURE);
    }

    clock_gettime(CLOCK_MONOTONIC, &g_start);
    pid_t pid = fork();
    if(pid < 0)
    {
        perror("Failed to fork");
        return EXIT_FAILURE;
    }
    else if(pid == 0)
    {
        execute_command(cmd, log_path);
    }

    char buffer[BUFFER_SIZE] = {};
    ssize_t total_bytes_written = 0;
    int status = -1;

    struct pollfd pfd = {
        .fd = log_fd,
        .events = POLLIN | POLLERR | POLLHUP
    };

    // Continue to read until the process exits
    while(!child_exited)
    {
        //fprintf(stderr, "poll start\n");
        if(poll(&pfd, 1, -1) > 0)
        {
            //fprintf(stderr, "poll stop\n");
            if(pfd.revents & POLLIN)
            {
                ssize_t forwarded = forward_data(log_fd, buffer, &total_bytes_written);
                // If we read no bytes, it's because we've reached
                // EOF. Unfortunately the semantics of poll() are such
                // that being at EOF is considered readable, so poll
                // will return right away. To properly block waiting
                // for more data we would need to use inotify, but
                // we're lazy, so just sleep so we don't burn up a
                // core while waiting for the other process to log
                // more.
                if(forwarded == 0 && !child_exited)
                {
                    struct timespec duration;
                    duration.tv_sec = 0;
                    duration.tv_nsec = 10000000; // 10ms
                    nanosleep(&duration, NULL);
                }
            }
            if(pfd.revents & (POLLERR | POLLHUP))
            {
                fprintf(stderr, "Error tailing fd!\n");
                break;
            }
        }
        else
        {
            perror("poll");
            exit(EXIT_FAILURE);
        }
        if(child_exited)
        {
            break;
        }
    }

    if(LOG_DEBUG) fprintf(stderr, "Waiting for child!\n");

    if(waitpid(pid, &status, WNOHANG) < 0)
    {
        perror("waitpid");
        exit(EXIT_FAILURE);
    }
    clock_gettime(CLOCK_MONOTONIC, &g_end);

    struct stat buf;
    fstat(log_fd, &buf);
    off_t size = buf.st_size;

    while(total_bytes_written < size)
    {
        if(LOG_DEBUG) fprintf(stderr, "Getting any bytes left %ld %ld!\n", total_bytes_written, size);
        forward_data(log_fd, buffer, &total_bytes_written);
    }

    if(LOG_DEBUG) fprintf(stderr, "Done!\n");

    double elapsed_time = (g_end.tv_sec - g_start.tv_sec) + (g_end.tv_nsec - g_start.tv_nsec) / 1e9;
    fprintf(stderr, "Duration: %f\n", elapsed_time);

    close(log_fd);

    // exit with same code as underlying command
    return WEXITSTATUS(status);
}