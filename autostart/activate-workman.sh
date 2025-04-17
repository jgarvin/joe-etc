#!/usr/bin/env bash

set -e

# thinkpad built in keyboard only
#DEVICE="AT Translated Set 2 keyboard"
ORBITOUCH_KEYBOARD="Blue Orb, Inc. orbiTouch Keyboard"
ORBITOUCH_KEYBOARD_ID=$(xinput -list | grep -i key | grep "$ORBITOUCH_KEYBOARD" | grep -Eo "id=[0-9]+" | cut -d= -f 2)
CHARACODER_KEYBOARD="CharaChorder CharaChorder 1 Keyboard"
CHARACODER_KEYBOARD_ID=$(xinput -list | grep -i key | grep "$CHARACODER_KEYBOARD" | grep -Eo "id=[0-9]+" | cut -d= -f 2)
ORBITOUCH_MOUSE="Blue Orb, Inc. orbiTouch Mouse"
ORBITOUCH_MOUSE_ID=$(xinput -list | grep -i key | grep "$ORBITOUCH_MOUSE" | grep -Eo "id=[0-9]+" | cut -d= -f 2)

for i in 1 2; do
    # xdotool only works correctly if you set the layout without setting the orbitouch_keyboard
    # so we set a generally for everything first
    setxkbmap -v workman -option caps:ctrl_modifier
    if xinput -list | grep orbiTouch &> /dev/null; then
        # then override the setting to correct it just for the orbitouch
        setxkbmap -device "$ORBITOUCH_KEYBOARD_ID" us
        # increase the mouse speed
        xinput --set-prop "Blue Orb, Inc. orbiTouch Mouse" "Coordinate Transformation Matrix" 3.400000, 0.000000, 0.000000, 0.000000, 3.400000, 0.000000, 0.000000, 0.000000, 1.000000
    fi
    if xinput -list | grep CharaChorder| &> /dev/null; then
        # then override the setting to correct it just for the orbitouch
        setxkbmap -device "$CHARACODER_KEYBOARD_ID" us
    fi
    xset r 40
    sleep 5 # sometimes first try doesn't stick because gnome overwrites at startup
done