#!/bin/bash

cp $(dirname "${BASH_SOURCE[0]}")/workman-p /usr/share/X11/xkb/symbols

localectl set-x11-keymap workman-p
