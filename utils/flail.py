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

import argparse

app_description = ("Automatically finds the most recent log and "
                   "monitors it with less.")

DEFAULT_INTERVAL = 5

parser = argparse.ArgumentParser(description=app_description)
parser.add_argument('-u', '--user', metavar='USER', type=str,
                    dest="user", help='Only open logs from USER.')
parser.add_argument('-l', '--lock', dest='lock', action='store_true',
                    default=False,
                    help="Don't automatically move to most recent log over time.")
parser.add_argument('-i', '--interval', metavar='T', type=int,
                    default=None,
                    dest="interval", help=("Poll for new logs every T seconds. "
                                           "Defaults to %d." % DEFAULT_INTERVAL))
parser.add_argument('-n', '--nth', metavar='N', type=int,
                    default=1,
                    dest="nth", help=("Open nth most recent log. Default 1."))
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

if args.nth <= 0:
    print >> sys.stderr, "flail: error: Nth must be 1 or greater."
    sys.exit(1)

if not args.interval:
    args.interval = DEFAULT_INTERVAL

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
    if args.nth > len(logfiles):
        print >>sys.stderr, "flail: error: No %dth most recent log file." % args.nth
        sys.exit(1)
    return logfiles[args.nth - 1][1]

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
            os.kill(old_less_instance.pid, signal.SIGKILL)
            try:
                while 1:
                    old_less_instance.wait()
                    break
            except OSError:
                pass
        less_instance = subprocess.Popen(monitor_cmd(current_file), shell=True)

def forward_signals(signum, frame):
    os.kill(less_instance.pid, signum)

check_for_newer()

def on_child_death(signum, frame):
    if less_instance:
        try:
            while 1:
                old_less_instance.wait()
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
