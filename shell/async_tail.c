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
#include <sys/prctl.h>

/* async_tail is a wrapper that redirects your command to a log and
 * launches a tail that exits only when all output is printed. This
 * way you run `async_tail --log /tmp/foo -- cmd_a | cmd_b` the speed
 * of cmd_a is not throttled by cmd_b's consumption rate. */

#define BUFFER_SIZE 4096
#define LOG_DEBUG 0

struct timespec g_start, g_end;

static ssize_t forward_data(int log_fd, char* buffer, ssize_t* total_bytes_written)
{
    ssize_t bytes_read = read(log_fd, buffer, BUFFER_SIZE);

    if(bytes_read < 0)
    {
        perror("Failed to read from log file");
        exit(EXIT_FAILURE);
    }

    ssize_t bytes_written_of_this_read = 0;
    while(bytes_written_of_this_read < bytes_read)
    {
        ssize_t bytes_written = write(STDOUT_FILENO, buffer, bytes_read);
        if (bytes_written < 0)
        {
            perror("write");
            exit(EXIT_FAILURE);
            continue;
        }
        bytes_written_of_this_read += bytes_written;
        *total_bytes_written += bytes_written;
    }

    return bytes_read;
}

volatile pid_t pid = -1;

static void forward_signal(int signum)
{
    kill(-pid, signum);
    exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
    struct sigaction sa;
    sa.sa_handler = SIG_DFL;
    sa.sa_flags = SA_RESTART | SA_NOCLDSTOP; // restart syscalls, don't notify on stopped child, only on exit
    sigemptyset(&sa.sa_mask);
    if (sigaction(SIGCHLD, &sa, NULL) == -1) {
        perror("sigaction");
        exit(EXIT_FAILURE);
    }
    sa.sa_handler = forward_signal;
    sa.sa_flags = SA_RESTART | SA_NOCLDSTOP; // restart syscalls, don't notify on stopped child, only on exit
    sigemptyset(&sa.sa_mask);
    if (sigaction(SIGINT, &sa, NULL) == -1) {
        perror("sigaction");
        exit(EXIT_FAILURE);
    }
    if (sigaction(SIGTERM, &sa, NULL) == -1) {
        perror("sigaction");
        exit(EXIT_FAILURE);
    }
    if (sigaction(SIGQUIT, &sa, NULL) == -1) {
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

    // have to open for writing first to make sure file exists
    int log_write_fd = open(log_path, O_WRONLY | O_CREAT | O_TRUNC, 0666);
    if (log_write_fd < 0)
    {
        perror("Failed to open log file");
        exit(EXIT_FAILURE);
    }

    int log_read_fd = open(log_path, O_RDONLY | O_NONBLOCK | O_CLOEXEC);
    if (log_read_fd < 0)
    {
        perror("Failed to open log file for reading");
        exit(EXIT_FAILURE);
    }

    int pgrp_notify_pipe[2] = {};
    if(pipe(pgrp_notify_pipe) < 0)
    {
        perror("pipe");
        exit(EXIT_FAILURE);
    }

    clock_gettime(CLOCK_MONOTONIC, &g_start);
    pid = fork();
    if(pid < 0)
    {
        perror("Failed to fork");
        return EXIT_FAILURE;
    }
    else if(pid == 0)
    {
        sa.sa_flags = 0;

        // undo being set in parent
        if (sigaction(SIGCHLD, &sa, NULL) == -1)
        {
            perror("signal");
            exit(EXIT_FAILURE);
        }

        // if async_tail dies before the child, shut it down
        if(prctl(PR_SET_PDEATHSIG, SIGKILL) < 0)
        {
            perror("prctl");
            abort();
        }

        // Create a process group so parent can wait on the group. If the
        // underlying command doesn't try to create any process groups
        // itself then all its children recursively will belong to the
        // same group, and we won't assess the file size until they are
        // all gone.
        int err = setpgrp();
        if(write(pgrp_notify_pipe[1], "\0", 1) < 0)
        {
            perror("write");
            exit(EXIT_FAILURE);
        }

        if(err < 0)
        {
            perror("setpgrp");
            exit(EXIT_FAILURE);
        }


        if(dup2(log_write_fd, STDOUT_FILENO) < 0)
        {
            perror("dup2");
            exit(EXIT_FAILURE);
        }
        if(dup2(log_write_fd, STDERR_FILENO) < 0)
        {
            perror("dup2");
            exit(EXIT_FAILURE);
        }
        close(log_write_fd);
        close(log_read_fd);

        execvp(cmd[0], cmd);
        perror("Failed to execute command");
        exit(EXIT_FAILURE);
    }

    close(log_write_fd);

    char buffer[BUFFER_SIZE] = {};

    // block until we are sure the child has run setpgrp
    if(read(pgrp_notify_pipe[0], buffer, 1) < 0)
    {
        perror("read");
        exit(EXIT_FAILURE);
    }

    ssize_t total_bytes_written = 0;
    int status = -1;

    struct pollfd pfd = {
        .fd = log_read_fd,
        .events = POLLIN | POLLERR | POLLHUP
    };

    int exit_code = -1;

    int sleep_on_next = 0;
    while(1)
    {
        if(poll(&pfd, 1, -1) > 0)
        {
            // Check if any processes in the subcommand's process group are left
            pid_t exited_pid = waitpid(-pid, &status, WNOHANG);
            if(exited_pid == pid)
            {
                exit_code = WEXITSTATUS(status);
            }
            if(exited_pid < 0)
            {
                break;
            }

            if(sleep_on_next)
            {
                sleep_on_next = 0;
                struct timespec duration;
                duration.tv_sec = 0;
                duration.tv_nsec = 10000000; // 10ms
                nanosleep(&duration, NULL);
            }

            if(pfd.revents & POLLIN)
            {
                ssize_t forwarded = forward_data(log_read_fd, buffer, &total_bytes_written);
                // If we read no bytes, it's because we've reached
                // EOF. Unfortunately the semantics of poll() are such
                // that being at EOF is considered readable, so poll
                // will return right away. To properly block waiting
                // for more data we would need to use inotify, but
                // we're lazy, so just sleep so we don't burn up a
                // core while waiting for the other process to log
                // more.
                if(forwarded == 0)
                {
                    // set a flag to sleep next iteration instead of
                    // sleeping directly here so the code checking if
                    // the children are still running has a chance to
                    // run again first
                    sleep_on_next = 1;
                    continue;
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
    }

    if(LOG_DEBUG) fprintf(stderr, "Waiting for child!\n");

    clock_gettime(CLOCK_MONOTONIC, &g_end);

    struct stat buf;
    fstat(log_read_fd, &buf);
    off_t size = buf.st_size;

    while(total_bytes_written < size)
    {
        if(LOG_DEBUG) fprintf(stderr, "Getting any bytes left %ld %ld!\n", total_bytes_written, size);
        forward_data(log_read_fd, buffer, &total_bytes_written);
    }

    if(LOG_DEBUG) fprintf(stderr, "Done!\n");

    double elapsed_time = (g_end.tv_sec - g_start.tv_sec) + (g_end.tv_nsec - g_start.tv_nsec) / 1e9;
    fprintf(stderr, "Duration: %f\n", elapsed_time);

    close(log_read_fd);

    // exit with same code as underlying command
    return exit_code;
}
