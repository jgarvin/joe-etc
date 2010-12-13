#!/usr/bin/env python

import glob
import sys
import os
import stat
import os.path as op
import signal
import subprocess
import pwd

import argparse

app_description = ("Automatically finds the most recent log and "
                   "monitors it with less.")

parser = argparse.ArgumentParser(description=app_description)
parser.add_argument('-u', '--user', metavar='USER', type=str,
                    dest="user", help='Only open logs from USER.')
parser.add_argument('-l', '--lock', dest='lock', action='store_true',
                    default=False,
                    help="Don't automatically move to most recent log over time.")
parser.add_argument('-i', '--interval', metavar='T', type=int,
                    default=None,
                    dest="interval", help='Poll for new logs every T seconds.')
parser.add_argument(metavar='APPLICATION', type=str,
                    dest="application",
                    help='Open logs for this application. Treated as substring.')

args = parser.parse_args()

if args.lock and args.interval != None:
    print >> sys.stderr, "flail: error: Interval is meaningless when locked."
    sys.exit(1)

if args.interval != None and args.interval <= 0:
    print >> sys.stderr, "flail: error: Interval must be 1 or greater."
    sys.exit(1)

if not args.interval:
    args.interval = 5

def get_log_files():
    glob_prefix = "/var/tmp/tlapp.*"
    if args.application:
        glob_path = glob_prefix + args.application + "*.log"
    else:
        glob_path = glob_prefix + ".log"

    logfilepaths = glob.glob(glob_path)

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
    return "less +F " + f

def latest_file():
    logfiles = get_log_files()
    if not logfiles:
        print >>sys.stderr, "flail: error: No matching log file found!"
        sys.exit(1)
    return logfiles[0][1]

def monitor_file(f):
    print "Running: " + monitor_cmd()
    return os.system(monitor_cmd())

current_file = None
less_instance = None

def check_for_newer(signum, frame):
    global current_file, less_instance
    candidate = latest_file()
    if candidate != current_file:
        current_file = candidate

        if less_instance:
            less_instance.terminate()
        less_instance = subprocess.Popen(monitor_cmd(current_file), shell=True)

def forward_signals(signum, frame):
    os.kill(less_instance.pid, signum)

check_for_newer(None, None)

if not args.lock:
    signal.signal(signal.SIGALRM, check_for_newer)
    signal.alarm(args.interval)

# Without this less doesn't exit correctly.
signal.signal(signal.SIGINT, forward_signals)

while 1:
    try:
        less_instance.wait()
        sys.exit(less_instance.returncode)
    except OSError:
        pass

sys.exit(1)
