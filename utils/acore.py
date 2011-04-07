#!/usr/bin/env python

import sys
import os
import grp
import pwd
import os.path as op
import getpass
import glob
import datetime
import popen2

# TODO: -s to just list cores
# TODO: Map /opt/tradelink/bin whichVersion scripts to their
# symlinks in /opt/tradelink/bin/newInst

def extract_name(core_path):
    "Extracts the name of an app from the name of its core filename."
    fname = op.basename(core_path)

    tmp = fname.strip("core.")
    core_name_parts = tmp.rsplit('.', 1)

    if len(core_name_parts) < 2:
        print >> sys.stderr, ("Couldn't figure out name of app based on filename "
                              "of core %s" % core_path)
        sys.exit(1)

    return core_name_parts[0]

def split_versioned_name(app_name):
    return app_name.rsplit('-', 1)

def find_scratchhost_binary(app_name):
    "Returns the path to the binary on scratchhost if present, otherwise None"
    name, version = split_versioned_name(app_name)
    bin_path = "/net/scratchhost/export/builds/*" + name + "*-" + version
    scratchList = glob.glob(bin_path)
    if len(bin_path) and op.exists(scratchList[0]):
        return scratchList[0]

    return None

def name_contains_version(app_name):
    """Returns True if the given name contains a tradelink version number,
    e.g. 'foo-1.2' will return True but 'foo' will not."""
    hyphen_split = app_name.rsplit('-', 1)
    if len(hyphen_split) == 1:
        return False

    period_split = hyphen_split[1].split('.')
    if len(period_split) == 1:
        return False

    return True

def run(command):
    the_run = popen2.Popen3(command)
    the_run.wait()
    return the_run.fromchild.read()

def time_str(timestamp):
    core_time = datetime.datetime.fromtimestamp(timestamp)
    return core_time.strftime("%c")

if "-h" in sys.argv or "--help" in sys.argv:
    print "acore by Joe G."
    print
    print "Automatically finds the most recent core and opens it in gdb."
    print "Usage: acore [-r] [appname]"
    print
    print "-h, --help\t Print this help"
    print "-r, --recent\t Choose most recent core without asking."
    sys.exit(0)

if len(sys.argv) > 2:
    print >> sys.stderr, "Only takes one argument, the application name."
    sys.exit(1)

coreList = glob.glob("/var/core/core.*")

if len(coreList) == 0:
    print >> sys.stderr, "There are no cores in /var/core to open."
    sys.exit(1)

if len(sys.argv) == 1:
    # If a specific app wasn't requested, see if all available cores are for
    # the same app.
    core_names = [extract_name(core) for core in coreList]
    if len(set(core_names)) == 1:
        chosenCoreName = core_names[0]
    else:
        print >> sys.stderr, ("There are cores from multiple apps available, "
                              "specify the core you want: ")
        for core in coreList:
            print >> sys.stderr, extract_name(core)
        sys.exit(1)
else:
    chosenCoreName = sys.argv[1]

possibleCores = glob.glob("/var/core/core.*" + chosenCoreName + "*.*")

if len(possibleCores) == 0:
    print >> sys.stderr, "No core for %s was found." % sys.argv[1]
    sys.exit(1)
elif len(possibleCores) == 1:
    chosenCore = [possibleCores[0], op.getmtime(possibleCores[0])]
else:
    possibleCores = [[core, op.getmtime(core)] for core in possibleCores]
    possibleCores.sort(key=lambda x: x[1], reverse=True)

    if "-r" in sys.argv or "--recent" in sys.argv:
        choice = 0
    else:
        print "Multiple cores were found. Which do you want?"
        print "(Just press enter for the most recent)"

        while 1:
            for i, (core, timestamp) in enumerate(possibleCores):
                print "%d. %s (%s)" % (i, core, time_str(timestamp))
            print "> ",

            choice = raw_input()

            if choice == '':
                choice = 0
            else:
                try:
                    choice = int(choice)
                except ValueError:
                    print >> sys.stderr, ("'%s' is not an integer. Enter a number "
                                          "from the list." % choice)
                    continue

                if choice < 0 or choice > len(possibleCores) - 1:
                    print >> sys.stderr, ("Please enter a choice between %d and "
                                          "%d." % (0, len(possibleCores) - 1))
                    continue

            break

    chosenCore = possibleCores[choice]

chosenBinary = None
if name_contains_version(chosenCoreName):
    # First try looking on scratchhost for the official unstripped binary.
    scratchhost_binary = find_scratchhost_binary(chosenCoreName)
    if scratchhost_binary:
        chosenBinary = scratchhost_binary

if not chosenBinary:
    # Inspect the core to see if we can find the binary.
    pargs_command = "pargs " + chosenCore[0]
    pargs_output = run(pargs_command)
    pargs_output = pargs_output.split("\n")

    pargs_error = ("Error, don't know where to look for binary on "
                   "scratchhost and couldn't extract the path to the "
                   "binary using pargs. "
                   "Failed pargs command: %s" % pargs_command)

    if len(pargs_output) < 2:
        print "error 1 %s " % pargs_output
        print >> sys.stderr, pargs_error
        sys.exit(1)

    binary_path = pargs_output[1].split(':', 1)
    if len(binary_path) == 1:
        print >> sys.stderr, pargs_error
        sys.exit(1)

    binary_path = binary_path[1].strip()
    if op.isabs(binary_path):
        chosenBinary = os.path.realpath(binary_path)
    else:
        # Use strings to get the PWD from the core, which we can join with
        # the relative path in argv[0] to locate the binary.
        # At the same, time, see if the '_' env variable is defined, which is
        # set by shells just before they execute external commands. WM's
        # launching apps won't always set it, but manual runs in the shell will.
        # It's the most reliable.
        strings_command = "strings " + chosenCore[0] + " | egrep '^(_=|PWD=)'"
        strings_output = run(strings_command)
        strings_output = strings_output.split("\n")

        def good_candidate(candidate):
            return op.exists(candidate) and not op.isdir(candidate)

        candidate = None
        for line in strings_output:
            if line.strip().startswith("_="):
                candidate = line.strip().split("=", 1)[1]
                if good_candidate(candidate):
                    chosenBinary = candidate
                    break
            if line.strip().startswith("PWD="):
                directory = line.strip().split("=", 1)[1]
                candidate = op.normpath(op.join(directory, binary_path))
                if good_candidate(candidate):
                    chosenBinary = candidate
                    break

        if not chosenBinary:
            print >> sys.stderr, "Unable to locate binary for core."
            print >> sys.stderr, "No binary found on scratchhost."
            print >> sys.stderr, "Extracted argv[0] from core: %s" % binary_path
            if candidate:
                print >> sys.stderr, "Found unsuitable candidate: %s" % candidate
                print >> sys.stderr, "Candidate either didn't exist or was a directory."
            sys.exit(1)

if name_contains_version(op.basename(chosenBinary)):
    # First try looking on scratchhost for the official unstripped binary.
    scratchhost_binary = find_scratchhost_binary(op.basename(chosenBinary))
    if scratchhost_binary:
        chosenBinary = scratchhost_binary

# A script runs periodically that changes the group for cores in /var/core
# to be corefans instead of root, but we can't trust it's configured correctly
# for all hosts. If the binary is less than 500MB, copy it to /export/home/$USER
# and open it there, otherwise warn and ask if we should copy it anyway.
try:
    local_home = op.join("/export/home", getpass.getuser())
    local_core_folder = op.join(local_home, "cores")
    core_megs = op.getsize(chosenCore[0]) / 1024. / 1024.
    agree = True
    if core_megs > 500:
        print >> sys.stderr, ("Warning: Core file is large (%dMB).\n\tStill copy to "
                              "%s? [yn]" % (core_megs, local_core_folder))
        agree = raw_input()
        agree = 'y' == agree or 'Y' == agree

    if agree:
        if not op.exists(local_home):
            os.mkdir(local_home)
        if not op.exists(local_core_folder):
            os.mkdir(local_core_folder)

        # Append the epoch timestamp on to the end to make the file unique
        target_core_name = op.basename(chosenCore[0]) + "." + str(chosenCore[1])
        target_core_path = op.join(local_core_folder, target_core_name)

        copy_command = "cp -p " + chosenCore[0] + " " + target_core_path
        if os.WEXITSTATUS(os.system(copy_command)):
            print >> sys.stderr, "Error, copy failed."
            print >> sys.stderr, "Copy command was: " + copy_command
            sys.exit(1)

        chosenCore[0] = target_core_path
except OSError:
    # We probably don't have permissions.
    pass

print "Core timestamp: " + time_str(chosenCore[1])
print "Binary timestamp: " + time_str(op.getmtime(chosenBinary))

gdb_command = "gdb " + chosenBinary + " " + chosenCore[0]
print "Running: " + gdb_command

sys.exit(os.WEXITSTATUS(os.system(gdb_command)))
