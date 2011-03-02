#!/usr/bin/env python

import glob
import sys
import os
import stat
import os.path as op
import signal
import subprocess
import pwd
import time
import socket

import argparse

# TODO: Get logs from /opt/tradelink/share/repository
# TODO: Get log from repository when new log is empty, switch when it becomes nonempty
# TODO: Debug why less doesn't get killed correctly

# TODO: Snake logs
# TODO: nic logs
# TODO: top logs

app_description = ("Automatically finds the most recent log and "
                   "monitors it with less.")

DEFAULT_INTERVAL = 1

parser = argparse.ArgumentParser(description=app_description)
parser.add_argument('-u', '--user', metavar='USER', type=str,
                    dest="user", help='Only open logs from USER.')
parser.add_argument('-l', '--lock', dest='lock', action='store_true',
                    default=False,
                    help="Don't automatically move to most recent log over time.")
parser.add_argument('-s', '--show-only', dest='show_only', action='store_true',
                    default=False,
                    help="Only show filenames for founds logs, don't open them.")
parser.add_argument('-i', '--interval', metavar='T', type=int,
                    default=None,
                    dest="interval", help=("Poll for new logs every T seconds. "
                                           "Defaults to %d." % DEFAULT_INTERVAL))
parser.add_argument('-n', '--nth', metavar='N', type=int,
                    default=None,
                    dest="nth", help=("Open nth most recent log. Default 1."))
parser.add_argument(metavar='APPLICATION', type=str,
                    dest="application",
                    help='Open logs for this application. Treated as substring.')

args = parser.parse_args()

def desired_log_number():
    if args.nth:
        return args.nth
    else:
        return 1

if args.lock and args.interval != None:
    print >> sys.stderr, "flail: error: Interval is meaningless when locked."
    sys.exit(1)

if args.interval != None and args.interval <= 0:
    print >> sys.stderr, "flail: error: Interval must be 1 or greater."
    sys.exit(1)

if desired_log_number() <= 0:
    print >> sys.stderr, "flail: error: Nth must be 1 or greater."
    sys.exit(1)

if not args.interval:
    args.interval = DEFAULT_INTERVAL

def get_glob_dirs(application):
    glob_prefixes = ["/var/tmp/"]

    if application:
        glob_app_dirnames = "/opt/tradelink/share/repository/*" + application + "*"
        glob_prefixes.extend(glob.glob(glob_app_dirnames))

    return glob_prefixes

def get_log_files():
    logfilepaths = []
    for d in get_glob_dirs(args.application):
        glob_prefix = "tlapp.*"
        if args.application:
            glob_path = op.join(d, "tlapp.*" + args.application + "*." + socket.gethostname() + "*.log")
        else:
            glob_path = op.join(d, "tlapp.*" + socket.gethostname() + "*.log")

        logfilepaths.extend(glob.glob(glob_path))

    logfilepaths = [l for l in logfilepaths if not op.isdir(l)]

    logpairs = [(os.stat(f), f) for f in logfilepaths]

    if args.user:
        filtered = []
        for f in logpairs:
            if pwd.getpwuid(f[0][stat.ST_UID])[0] == args.user:
                filtered.append(f)
        logpairs = filtered

    logpairs.sort(key=lambda x: x[0][stat.ST_MTIME], reverse=True)
    return logpairs

def monitor_cmd(f):
    return "less -n +F " + f

def latest_file():
    logfiles = get_log_files()
    if not logfiles:
        print >>sys.stderr, "flail: error: No matching log file found!"
        sys.exit(1)
    if desired_log_number() > len(logfiles):
        print >>sys.stderr, "flail: error: No %dth most recent log file." % desired_log_number()
        sys.exit(1)
    return logfiles[desired_log_number() - 1][1]

def monitor_file(f):
    print "Running: " + monitor_cmd()
    return os.system(monitor_cmd())

current_file = None
less_instance = None
killing_child = False

def check_for_newer():
    global current_file, less_instance
    candidate = latest_file()
    if candidate != current_file:
        current_file = candidate

        old_less_instance = less_instance
        less_instance = None
        if old_less_instance:
            os.kill(old_less_instance.pid, signal.SIGCHLD)
            try:
                while 1:
                    old_less_instance.wait()
                    break
            except OSError:
                pass
        less_instance = subprocess.Popen(monitor_cmd(current_file), shell=True)

def forward_signals(signum, frame):
    global less_instance
    os.kill(less_instance.pid, signum)

if args.show_only:
    # We use args.nth directly instead of desired_log_number()
    # since a slice of 0:None maps to the whole container.
    tmp = [i[1] for i in get_log_files()][0:args.nth]
    tmp.reverse()
    print "\n".join(tmp)
    sys.exit(0)

check_for_newer()

def on_child_death(signum, frame):
    if less_instance:
        try:
            while 1:
                less_instance.wait()
                break
        except OSError:
            pass
        sys.exit(less_instance.returncode)

# Without this less doesn't exit correctly.
signal.signal(signal.SIGINT, forward_signals)
signal.signal(signal.SIGCHLD, on_child_death)

while 1:
    time.sleep(args.interval)
    less_instance.poll()
    if less_instance.returncode != None:
        sys.exit(less_instance.returncode)
    if not args.lock:
        check_for_newer()

sys.exit(1)
