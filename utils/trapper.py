#!/usr/bin/env python

# Runs its second argument, then waits for the first argument
# to appear on stdout. Until the first argument appears on stdout,
# pressing Ctrl-c will cause the child process to be killed. This
# works around apps that don't let you Ctrl-C while loading.
# I'm looking at you GDB...

# thanks to papna_ and squigle in #python for subprocess help

import sys
import signal
import subprocess
import select
import fcntl
import os

def deblockify(fd):
    fl = fcntl.fcntl(fd, fcntl.F_GETFL)
    fcntl.fcntl(fd, fcntl.F_SETFL, fl | os.O_NONBLOCK)

def main():
    enabled = True
    to_launch = None

    def siginthandler_kill(signum, frame):
        if to_launch and enabled:
            # Would use send_signal but this works in python2.4
            os.kill(to_launch.pid, signal.SIGKILL)

    listen_for = sys.argv[1]
    to_run = sys.argv[2:]

    signal.signal(signal.SIGINT, siginthandler_kill)

    # Just launch if given no arguments
    if len(sys.argv) == 2:
        sys.exit(os.WEXITSTATUS(os.system("".join(to_run))))

    to_launch = subprocess.Popen(to_run,
                                 bufsize=1,
                                 stdout=subprocess.PIPE)

    child_stdout = to_launch.stdout

    poller = select.poll()
    poller.register(child_stdout, select.POLLIN)

    # Even though I don't read from this, I need to poll on it
    # in order for newlines to interleave correctly with stdout.
    poller.register(sys.stdin, select.POLLIN)

    deblockify(child_stdout.fileno())

    listen_for_index = 0

    stdout_fd = child_stdout.fileno()

    while to_launch.returncode == None:
        try:
            to_launch.poll()
            event_list = []
            event_list = poller.poll()

            for fd, event in event_list:
                if fd == stdout_fd:
                    output = child_stdout.read()

                    if enabled:
                        for o in output:
                            if o == listen_for[listen_for_index]:
                                listen_for_index += 1
                                if listen_for_index == len(listen_for):
                                    signal.signal(signal.SIGINT, signal.SIG_IGN)
                                    enabled = False
                                    break
                            else:
                                listen_for_index = 0

                    sys.stdout.write(output)
                    sys.stdout.flush()
                    break
        # Interrupted system calls are fine, when the child dies
        # we'll break out of the loop.
        except IOError:
            pass
        except select.error:
            pass

    sys.exit(to_launch.returncode)

if __name__ == "__main__":
    main()

