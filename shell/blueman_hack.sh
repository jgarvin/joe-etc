#!/usr/bin/env bash

# Taken from https://github.com/blueman-project/blueman/issues/2282
# the issue is that for some reason the first start never draws the icon right

# Initial attempt to start the blueman-applet
blueman-applet &
# Sleep for 5 seconds to make sure attempt executed
sleep 5
# Do the first check and kill the first instance that fails to draw icon
if pgrep -x "blueman-applet" >/dev/null; then
	pkill -x "blueman-applet"
	sleep 2
fi
# tragain
blueman-applet &
# Check if ok and if ok exit before the loop
if pgrep -x "blueman-applet" >/dev/null; then
	exit 0
fi

# Init the counter to not loop indefinitely but do max of 10 attrempts
counter=0
# Loop to try and start the blueman-applet
while [ $counter -lt 10 ]; do
	# Try to start blueman-applet
	blueman-applet &
	# Sleep for 2 seconds before the next check...
	sleep 2
	# Check if success
	if pgrep -x "blueman-applet" >/dev/null; then
		exit 0
	# ... and prompt if attempt failed.
	else
		notify-send -u normal "Attempt $((counter + 1)) to start bluetooth tray applet failed."
	fi
	((counter++))
done

# Final check and if all fails then prompt
if ! pgrep -x "blueman-applet" >/dev/null; then
	notify-send -u critical "Couldn't start bluetooth tray applet!"
fi