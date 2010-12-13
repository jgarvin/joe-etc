#!/usr/bin/env python

import glob
import sys
import os
import stat
import os.path as op
import signal

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
                    help='Open logs for this application.')

args = parser.parse_args()

if args.lock and args.interval != None:
    print >> sys.stderr, "flail: error: Interval is meaningless when locked."
    sys.exit(1)

def get_log_files():
    glob_prefix = "/var/tmp/tlapp.*"
    if args.application:
        glob_path = glob_prefix + args.application + "*.log"
    else:
        glob_path = glob_prefix + ".log"

    logfilepaths = glob.glob(glob_path)

    logfilepaths = [l for l in logfilepaths if not op.isdir(l)]

    logpairs = [(os.stat(f)[stat.ST_MTIME], f) for f in logfilepaths]
    logpairs.sort(key=lambda x: x[0], reverse=True)
    return logpairs

def monitor_file(f):
    cmd = "less +F " + f
    print "Running: " + cmd
    return os.system(cmd)

if args.lock:
    sys.exit(os.WEXITSTATUS(monitor_file(get_log_files()[0][1])))

sys.exit(0)
