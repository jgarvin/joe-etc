#!/usr/bin/env python

import socket
import random
import sys
import string
import os

MY_BOX = "udesktop178"

if socket.gethostname() != MY_BOX:
    print "YOU ARE NOT RUNNING ON YOUR BOX. YOU ARE ATTEMPTING TO RUN: "
    print
    print " ".join(sys.argv[1:])
    print

    while 1:
        authentication_string = "".join([random.choice(string.letters) for i in range(3)])
        print "ARE YOU SURE? If so type the string \"%s\"" % authentication_string
        print ">",
        attempt = raw_input()
        if attempt != authentication_string:
            print "ACCESS DENIED."
            continue
        break

    print
    print "Running: " + sys.argv[1]
    print "With arguments: " + str(sys.argv[2:])
    print

os.execvp(sys.argv[1], sys.argv[1:])
