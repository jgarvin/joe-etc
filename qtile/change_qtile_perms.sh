#!/usr/bin/env bash

# Copy to /root and `chmod +x` it

set -x

for f in /sys/class/backlight/*/brightness /sys/class/leds/*/brightness /sys/class/power_supply/BAT*/charge_control_*_threshold; do
    chmod u+rw,g+rw,o+r $f
    chgrp sudo $f
done
