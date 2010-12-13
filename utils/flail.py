#!/usr/bin/env python

import glob
import os
import stat
import os.path as op

def time_files(file_list):
    logfilepaths = glob.glob("/var/tmp/tlapp.*.log")
    logpairs = [(os.stat(f)[stat.ST_MTIME], f) for f in logfilepaths]
    logpairs.sort(key=lambda x: x[0], reverse=True)
