#!/bin/bash

# Save original outputs configuration
original_config=$(swaymsg -t get_outputs -r)

# Find the largest non-rotated monitor
largest_monitor=$(wlr-randr --json | jq -r 'map(select(.transform == "normal")) | map({name: .name, area: (.physical_size.width * .physical_size.height)}) | sort_by(.area) | reverse | .[0].name')

if [ -z "$largest_monitor" ]; then
    echo "Error: Could not find any non-rotated monitor"
    exit 1
fi

echo "Largest non-rotated monitor: $largest_monitor"

# Disable all outputs except the largest
all_monitors=$(wlr-randr --json | jq -r '.[].name')
for monitor in $all_monitors; do
    if [ "$monitor" != "$largest_monitor" ]; then
        swaymsg output "$monitor" disable
    fi
done

# Set the largest monitor to position 0,0 (this prevents issue with
# mouse only being able to move over half the screen).
swaymsg output "$largest_monitor" position 0 0

echo "All outputs except $largest_monitor have been disabled and positioned at 0,0"

# Run the command
echo "Running command: $@"
"$@"
cmd_status=$?

# Restore original configuration
echo "Restoring original output configuration"
echo "$original_config" | jq -r '.[] | "swaymsg output \(.name) \(if .active then "enable" else "disable" end) position \(.rect.x) \(.rect.y)"' | bash

echo "Original configuration restored"
exit $cmd_status