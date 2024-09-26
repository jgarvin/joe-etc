#!/usr/bin/env python

from collections import defaultdict

# bind[unit][action][direction]
bind = defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: None)))

units = [
    'line', # u,d,r?,l? or two ident actions
    'word', # u?,d?,r,l or next/prev actions
    'symbol', # u?,d?,r,l or next/prev actions
    'group', # u,d,r,l
    'paragraph',
    'indent',
    'buffer'
]

# 'in' and 'out' directions? what do they mean for units other than group?
# is 'current' a direction?
dirs = [
    'left',
    'down',
    'up',
    'right'
]

# modifier determines unit
# what determines direction? if there were only two it could be tap count
# for movement, that's what we do
# only other direction is for barf/slurp, where we can have tap determine forward vs back
actions = [
    'move cursor to start of $unit',
    'move cursor to end of $unit',
    'move cursor by $unit $dir',
    'move unit $dir',
    'select $unit',
    'cut $unit',
    'copy $unit',
    'paste over $unit',
    'rewrap $unit', # will just add wrapping if there isn't any
    'barf $unit $dir',
    'slurp $unit $dir',
    'wrap $unit',
    'unwrap $unit',
    'split $unit'
]

mods = [
    'C-M',
    'C-S',
    'M-S',
    'C-M-S',
    's-C',
    's-M',
    's-C-M'
    # s and s-S are reserved for wayland/X11 window management
]

# sorted, easiest to hit first
keys = [
    't',
    'n',
    'h',
    'e',
    's',
    'o',
    'c',
    'l',
    'r',
    'u',
    'd',
    'p'
]

for unit in units:
    for action in actions:
        for direction in directions:
            if "$dir" not in action:
                continue


# what does it mean to move by paragraph to the left? nonsensical. only works for editing

# other smartparens actions:
# barf
# slurpk
# splitk
# splice/unwrap