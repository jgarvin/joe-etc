#!/usr/bin/env python

from __future__ import print_function

import sys
import os
import grp
import pwd
import os.path as op
import getpass
import glob
import datetime
import sh

# TODO: -s to just list cores
# TODO: Search for versions in logfiles (flail integration). Would
# help with apps whose name is too long.

def extract_name(core_path):
    "Extracts the name of an app from the name of its core filename."
    fname = op.basename(core_path)

    tmp = fname.strip("core.")
    core_name_parts = tmp.rsplit('.', 1)

    if len(core_name_parts) < 2:
        return "<Unknown>"

    return core_name_parts[0]

def split_versioned_name(app_name):
    return app_name.rsplit('-', 1)

def name_contains_version(app_name):
    """Returns True if the given name contains a version number,
    e.g. 'foo-1.2' will return True but 'foo' will not."""
    hyphen_split = app_name.rsplit('-', 1)
    if len(hyphen_split) == 1:
        return False

    period_split = hyphen_split[1].split('.')
    if len(period_split) == 1:
        return False

    return True

def run(command, clear_ld_env=True):
    old_env_vars = {}
    def backup_env_var(var):
        if os.environ.has_key(var):
            print("Warning: Unsetting %s for invoking %s" % (var, command), file=sys.stderr)
            old_env_vars[var] = os.environ[var]
            os.environ[var] = ''

    if clear_ld_env:
        backup_env_var("LD_PRELOAD")
        backup_env_var("LD_LIBRARY_PATH")

    the_run = sh.Command(command)
    the_run = the_run()
    
    for i in old_env_vars:
        os.environ[i] = old_env_vars[i]

    return the_run

def time_str(timestamp):
    core_time = datetime.datetime.fromtimestamp(timestamp)
    return core_time.strftime("%c")

if "-h" in sys.argv or "--help" in sys.argv:
    print("Automatically finds the most recent core and opens it in gdb.")
    print("Usage: acore [-r] [appname]")
    print()
    print("-h, --help\t Print this help")
    print("-r, --recent\t Choose most recent core without asking.")
    sys.exit(0)

if len(sys.argv) > 2:
    print("Only takes one argument, the application name.", file=sys.stderr)
    sys.exit(1)

coreList = glob.glob("/tmp/core.*") + glob.glob("./core.*") + glob.glob("/var/core.*") + glob.glob("/tmp/cores/core.*") + glob.glob("./core") + glob.glob("/var/tmp/*core*")

if len(coreList) == 0:
    print("Found no cores in /tmp or current directory.", file=sys.stderr)
    sys.exit(1)

if len(sys.argv) != 1:
    chosenCoreName = sys.argv[1]
    for c in coreList:
        if chosenCoreName in c:
            chosenCore = c

possibleCores = coreList

if len(possibleCores) == 0:
    print("No core for %s was found." % sys.argv[1], file=sys.stderr)
    sys.exit(1)
elif len(possibleCores) == 1:
    chosenCore = possibleCores[0]
else:
    possibleCores = [[core, op.getmtime(core)] for core in possibleCores]
    possibleCores.sort(key=lambda x: x[1])

    if "-r" in sys.argv or "--recent" in sys.argv:
        choice = 0
    else:
        print("Multiple cores were found. Which do you want?")
        print("(Just press enter for the most recent)")

        while 1:
            for i, (core, timestamp) in enumerate(possibleCores):
                print("%d. %s (%s)" % (len(possibleCores)-1-i, core, time_str(timestamp)))
            print("> ",end='')

            choice = raw_input()

            if choice == '':
                choice = len(possibleCores)-1
            else:
                try:
                    choice = len(possibleCores)-1-int(choice)
                except ValueError:
                    print("'%s' is not an integer. Enter a number "
                          "from the list." % choice, file=sys.stderr)
                    continue

                if choice < 0 or choice > len(possibleCores) - 1:
                    print("Please enter a choice between %d and "
                          "%d." % (0, len(possibleCores) - 1), file=sys.stderr)
                    continue

            break

    chosenCore = possibleCores[choice][0]

def findBinaryForCore(coreName):
    """Uses strings on the binary to find potential
    binary names, then picks the one with the most
    recent mtime, since we assume these cores are from
    development."""

    f = str(sh.file(coreName))
    looking_for = "execfn: '"
    index = f.find(looking_for)
    if index != -1:
        end_index = f.find("',", index + len(looking_for))
        if end_index != -1:
            return f[index + len(looking_for):end_index]

    strings = sh.strings(coreName).split("\n")
    strings = [s for s in strings if s]
    candidate = ("", 0)
    for s in strings:
        if op.exists(s):
            finfo = sh.file(s)
            if "ELF" in finfo and "executable" in finfo:
                mtime = op.getmtime(s)
                if mtime > candidate[1]:
                    candidate = (s, mtime)

    return candidate[0]

chosenBinary = findBinaryForCore(chosenCore)
if not chosenBinary:
    print("Couldn't find binary for core: " + chosenBinary, file=sys.stderr)
    sys.exit(1)

print("Core timestamp: " + time_str(op.getmtime(chosenCore)))
print("Binary timestamp: " + time_str(op.getmtime(chosenBinary)))

print("Chosen core: " + chosenCore)
print("Chosen binary: " + chosenBinary)

gdb_command = "gdb " + chosenBinary + " " + chosenCore
print("Running: " + gdb_command)

sys.exit(os.WEXITSTATUS(os.system(gdb_command)))
