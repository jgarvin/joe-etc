#!/usr/bin/env python

"""
Test: 0000000000000000000000000001000001000110100101100111000110000110
Value: 6 8 1 7 6 9 6 4 0 1
"""

import fileinput
import re

for line in fileinput.input():
    for nibble in re.findall("....", line)[::-1]:
        print int(nibble, 2),
print
