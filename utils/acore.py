#!/bin/env python

import sys
import os
import os.path as op
import glob
import datetime
import popen2

def extract_name(core_path):
    "Extracts the name of an app from the name of its core file."
    fname = op.basename(core_path)
    core_name_parts = fname.split('.')

    if len(core_name_parts) < 3:
        print >> sys.stderr, ("Couldn't figure out name of app based on filename "
                              "of core %s" % core_path)
        sys.exit(1)

    return core_name_parts[1]

def find_scratchhost_binary(app_name):
    "Returns the path to the binary on scratchhost if present, otherwise None"
    bin_path = op.join("/net/scratchhost/export/builds/", app_name)
    if op.exists(bin_path):
        return bin_path

    return None

def name_contains_version(app_name):
    """Returns True if the given name contains a tradlink version number,
    e.g. 'foo-1.2' will return True but 'foo' will not."""
    hyphen_split = app_name.rsplit('-', 1)
    if len(hyphen_split) == 1:
        return False

    period_split = hyphen_split('.')
    if len(period_split) == 1:
        return False

    return True

def run(command):
    the_run = popen2.Popen3(command)
    the_run.wait()
    return the_run.fromchild.read()

if "-h" in sys.argv or "--help" in sys.argv:
    print "acore by Joe G."
    print
    print "Automatically finds the most recent core and opens it in gdb."
    print "Usage: acore [appname]"
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

possibleCores = glob.glob("/var/core/core." + chosenCoreName + ".*")

if len(possibleCores) == 0:
    print >> sys.stderr, "No core for %s was found." % sys.argv[1]
    sys.exit(1)
elif len(possibleCores) == 1:
    chosenCore = (possibleCores[0], op.getmtime(possibleCores[0]))
else:
    possibleCores = [(core, op.getmtime(core)) for core in possibleCores]
    possibleCores.sort(key=lambda x: x[1], reverse=True)

    if "-r" in sys.argv or "--recent" in sys.argv:
        choice = 0
    else:
        print "Multiple cores were found. Which do you want?"
        print "(Just press enter for the most recent)"

        while 1:
            for i, (core, timestamp) in enumerate(possibleCores):
                core_time = datetime.datetime.fromtimestamp(timestamp)
                time_str = core_time.strftime("%c")
                print "%d. %s (%s)" % (i, core, time_str)
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
        print
        print binary_path
        print
        print pargs_output
        print
        print pargs_output[1]
        print
        print >> sys.stderr, pargs_error
        sys.exit(1)

    binary_path = binary_path[1].strip()
    if op.isabs(binary_path):
        chosenBinary = binary_path
    else:
        # Use strings to get the PWD from the core, which we can join with
        # the relative path in argv[0] to locate the binary.
        strings_command = "strings " + chosenCore[0] + " | grep PWD"
        strings_output = run(strings_command)
        strings_output = strings_output.split("\n")

        candidate = None
        for line in strings_output:
            if line.strip().startswith("PWD="):
                directory = line.strip().split("=", 1)[1]
                candidate = op.normpath(op.join(directory, binary_path))
                if op.exists(candidate) and not op.isdir(candidate):
                    chosenBinary = candidate
                    break

        if not chosenBinary:
            print >> sys.stderr, "Unable to locate binary for core."
            print >> sys.stderr, "No binary found on scratchhost."
            print >> sys.stderr, "Extracted argv[0] from core: %s" % binary_path
            if candidate:
                print >> sys.stderr, "Found unsuitable candidate: %s" % candidate
                print >> sys.stderr, "Candidate either didn't exist or was a directory."

print chosenCore
print chosenBinary

# Output the date and time of the core
# Output the path to the core
# Output the date and time of the app
# Output the path to the app

# Copy the core to /export/home/joeg

# Start GDB

