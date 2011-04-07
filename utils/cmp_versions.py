#!/usr/bin/env python

"""

Compares two version files, then outputs whether the latter
version represents a new major, minor, or micro.

Intended usage:

$ cat version.in
0.1.0
$ cat version.tmp
0.2.0
$ cmp_versions.py version.in version.tmp
minor

"""

import sys

if len(sys.argv) > 3:
    print >>sys.stderr, "Too many arguments."
    sys.exit(1)

if len(sys.argv) < 3:
    print >>sys.stderr, "Too few arguments, need two version files."

first = open(sys.argv[1]).readlines()
second = open(sys.argv[2]).readlines()

def strip_comments(x):
    new_contents = []
    for l in x:
        if l[0] == "#":
            continue
        new_contents.append(l)

    return '\n'.join(new_contents)

first = strip_comments(first)
second = strip_comments(second)

first = first.split('.')
second = second.split('.')

if len(first) != 3:
    print >>sys.stderr, "Malformatted version string in first file."
    sys.exit(1)

if len(second) != 3:
    print >>sys.stderr, "Malformatted version string in first file."
    sys.exit(1)

first = [int(i) for i in first]
second = [int(i) for i in second]

if second[0] > first[0]:
    print "major"
elif second[1] > first[1]:
    print "minor"
elif second[2] > first[2]:
    print "micro"
else:
    print >>sys.stderr, "Second version appears equal or older!"
    sys.exit(1)

sys.exit(0)

