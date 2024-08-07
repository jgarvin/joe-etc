#!/usr/bin/env bash

# We assume pip was already used to setup a virtualenv and install qtile like this:
#
# python3 -m venv ~/opt/qtile
# source ~/opt/qtile/bin/activate
# pip3 install qtile
#
# Also assume that you have setup a qtile-wayland.desktop file that
# invokes this script by copying it to /usr/share/wayland-sessions and
# replacing $USER appropriately inside.

source ~/opt/qtile/bin/activate
source ~/.xprofile

# Ubuntu puts this in a different location that qtile expects apparently
export LIBGL_DRIVERS_PATH=/usr/lib/x86_64-linux-gnu/dri/

exec qtile start -b wayland
