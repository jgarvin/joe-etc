#!/bin/bash

# next try X-GNOME-Autostart-Delay=10

sleep 5 # needed, otherwise setxkbmap has no effect! probably because gnome applies some other setting on top at startup
setxkbmap -v workman-p && xset r 66
