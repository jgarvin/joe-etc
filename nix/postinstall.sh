#!/usr/bin/env bash

if [ "$EUID" -ne 0 ]; then
    echo "Error: This script must be run as root" >&2
    exit 1
fi

nix-channel --add https://nixos.org/channels/nixos-unstable unstable

# if you add without doing the update then imports of unstable still
# don't work!
nix-channel --update