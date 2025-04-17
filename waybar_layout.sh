#!/usr/bin/env bash

env > /tmp/swaylog

exec swaymsg -t get_workspaces | jq -r ".[] | select(.visible and .output == \"$WAYBAR_OUTPUT_NAME\") | .representation"
