#!/usr/bin/python

"""Checks if files are binaries before catting them. Because I'm error prone."""

import string, sys
import os, stat
import os.path
import resource

# Taken from: http://stackoverflow.com/questions/377017/test-if-executable-exists-in-python
# All SO question and answers are under a CC license requiring attribution.
# Author: user 'Jay'
# User profile: http://stackoverflow.com/users/20840/jay
def which(program):
    import os
    def is_exe(fpath):
        return os.path.exists(fpath) and os.access(fpath, os.X_OK)

    fpath, fname = os.path.split(program)
    if fpath:
        if is_exe(program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            exe_file = os.path.join(path, program)
            if is_exe(exe_file):
                return exe_file

    return None



# Code to determine whether file is binary or not
# taken from: http://code.activestate.com/recipes/173220/
text_characters = "".join(map(chr, range(32, 127)) + list("\n\r\t\b"))
_null_trans = string.maketrans("", "")

def istextfile(filename, blocksize = 512):
    return istext(open(filename).read(blocksize))

def istext(s):
    if "\0" in s:
        return 0

    if not s:  # Empty files are considered text
        return 1

    # Get the non-text characters (maps a character to itself then
    # use the 'remove' option to get rid of the text characters.)
    t = s.translate(_null_trans, text_characters)

    # If more than 30% non-text characters, then
    # this is considered a binary file
    if float(len(t))/len(s) > 0.30:
        return 0
    return 1



if __name__ == "__main__":
    try:
        for arg in sys.argv:
            if arg == "-":
                continue
            if not stat.S_ISREG(os.stat(arg)[stat.ST_MODE]):
                continue

            if os.path.exists(arg):
                f = open(arg)
                contents = f.read(resource.getpagesize()) # Examine first page only
                f.close()
                if not istext(contents):
                    print >> sys.stderr, "Don't cat binaries!"
                    sys.exit(1)
    except IOError: # If missing file just give us the error from normal cat
        pass

    cat_prog = which("gcat")
    if not cat_prog:
        cat_prog = which("cat")

    os.execv(cat_prog, [os.path.basename(cat_prog)] + sys.argv[1:])
