#!/usr/bin/env bash

if [ -f /etc/vconsole.conf ]; then
    echo "/etc/vconsole.conf already exists!" >2
    exit 1
fi

mkdir -p /usr/local/share/kbd/keymaps/

cp $(dirname "${BASH_SOURCE[0]}")/workman-p.iso15.kmap /usr/local/share/kbd/keymaps/

echo "KEYMAP=/usr/local/share/kbd/keymaps/workmap-p.iso15.kmap" >> /etc/vconsole.conf
