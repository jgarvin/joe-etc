#!/usr/bin/env bash
set -euo pipefail

outputs="$(swaymsg --raw --type get_outputs)"
# Sunshine's HEADLESS outputs are not plugged-in displays.
external_available="$(
    jq --raw-output '
        if any(.[]; .name == "eDP-1") then
            any(.[];
                .name != "eDP-1"
                and (.name | startswith("HEADLESS-") | not)
            )
        else
            error("eDP-1 is missing from the Sway output list")
        end
    ' <<<"$outputs"
)"

if [[ "$external_available" == true ]]; then
    swaymsg --quiet output eDP-1 disable
fi
