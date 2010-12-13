#!/usr/bin/env python

import glob
import sys
import os
import stat
import os.path as op
import argparse

app_description = ("Automatically finds the most recent log and "
                   "monitors it with less.")

parser = argparse.ArgumentParser(description=app_description)
parser.add_argument('--user', metavar='USER', type=str,
                    dest="user", help='Only open logs from USER.')
parser.add_argument('--lock', dest='lock', action='store_true',
                    default=False,
                    help="Don't automatically move to most recent log over time.")
parser.add_argument(metavar='APPLICATION', type=str,
                    dest="application",
                    help='Open logs for this application.')

args = parser.parse_args()

def get_log_files():
    glob_prefix = "/var/tmp/tlapp.*"
    if args.application:
        glob_path = glob_prefix + args.application + "*.log"
    else:
        glob_path = glob_prefix + ".log"

    logfilepaths = glob.glob(glob_path)
    logpairs = [(os.stat(f)[stat.ST_MTIME], f) for f in logfilepaths]
    logpairs.sort(key=lambda x: x[0], reverse=True)
    return logpairs

def monitor_file(f):
    os.system("less +F " + f)

monitor_file(get_log_files()[0][1])

sys.exit(0)
