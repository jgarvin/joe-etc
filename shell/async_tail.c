#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sys/poll.h>
#include <errno.h>

#define BUFFER_SIZE 4096

static void execute_command(char *cmd[], char *log_path)
{
    int log_fd = open(log_path, O_WRONLY | O_CREAT | O_TRUNC, 0666);
    if (log_fd < 0)
    {
        perror("Failed to open log file");
        exit(EXIT_FAILURE);
    }

    dup2(log_fd, STDOUT_FILENO);
    dup2(log_fd, STDERR_FILENO);
    close(log_fd);

    execvp(cmd[0], cmd);
    perror("Failed to execute command");
    exit(EXIT_FAILURE);
}

static void forward_data(int log_fd, char* buffer, ssize_t* total_bytes_written)
{
    ssize_t bytes_read = 0;
    while((bytes_read = read(log_fd, buffer, BUFFER_SIZE)) > 0)
    {
        ssize_t bytes_written = write(STDOUT_FILENO, buffer, bytes_read);
        if (bytes_written < 0)
        {
            perror("write");
            exit(EXIT_FAILURE);
        }
        *total_bytes_written += bytes_written;
    }
    if(bytes_read < 0)
    {
        perror("Failed to read from log file");
        exit(EXIT_FAILURE);
    }
}

static void tail_log_file(int signal_fd, char *log_path, pid_t child_pid)
{
    int log_fd = open(log_path, O_RDONLY | O_NONBLOCK);
    if (log_fd < 0)
    {
        perror("Failed to open log file for reading");
        exit(EXIT_FAILURE);
    }

    char buffer[BUFFER_SIZE] = {};
    ssize_t bytes_read = 0;
    ssize_t bytes_written = 0;
    ssize_t total_bytes_written = 0;
    int status = -1;

    struct pollfd pfd[2] = {
        {
            .fd = signal_fd,
            .events = POLLIN
        },
        {
            .fd = log_fd,
            .events = POLLIN
        }
    };

    // Continue to read until the process exits
    while(1)
    {
        if(poll(pfd, 2, -1) > 0)
        {
            if(pfd[0].revents & POLLIN)
            {
                break;
            }
            forward_data(log_fd, buffer, &total_bytes_written);
        }
    }

    while(bytes_written < total_bytes_written)
    {
        forward_data(log_fd, buffer, &total_bytes_written);
    }

    close(log_fd);
    close(signal_fd);
}

int main(int argc, char *argv[])
{
    if(argc < 5 || strcmp(argv[1], "--log") != 0 || strcmp(argv[3], "--") != 0) {
        fprintf(stderr, "Usage: %s --log <log_path> -- <command>\n", argv[0]);
        return EXIT_FAILURE;
    }

    char *log_path = argv[2];
    char **cmd = &argv[4];

    sigset_t mask;
    sigemptyset(&mask);
    sigaddset(&mask, SIGCHILD);
    if(sigprocmask(SIG_BLOCK, &mask, NULL) < 0)
    {
        perror("sigprocmask");
        exit(EXIT_FAILURE);
    }

    int sfd = signalfd(-1, &mask, SFD_CLOEXEC);
    if(sfd < 0)
    {
        perror("signalfd");
        exit(EXIT_FAILURE);
    }

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
    else
    {
        tail_log_file(sfd, log_path, pid);
        wait(NULL);
    }

    return EXIT_SUCCESS;
}
