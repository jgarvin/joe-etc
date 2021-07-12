#!/bin/bash

# directions here:
# https://makandracards.com/makandra-orga/473864-how-to-set-a-fixed-microphone-source-in-ubuntu-if-it-keeps-changing

# ~$ pactl list short sources
# 1	alsa_output.pci-0000_00_1f.3.analog-stereo.monitor	module-alsa-card.c	s16le 2ch 48000Hz	SUSPENDED
# 2	alsa_input.pci-0000_00_1f.3.analog-stereo	module-alsa-card.c	s16le 2ch 44100Hz	SUSPENDED
# 12	alsa_input.usb-DPA_Microphones_d_vice_MMA-A_17060A06002B6300-00.iec958-stereo	module-alsa-card.c	s24le 2ch 48000Hz	RUNNING
# 15	alsa_input.usb-Lenovo_ThinkPad_Thunderbolt_3_Dock_USB_Audio_000000000000-00.mono-fallback	module-alsa-card.c	s16le 1ch 48000Hz	SUSPENDED

if pactl list short sources | grep DPA_Microphones_d_vice; then
    pactl set-default-source "$(pactl list short sources | grep DPA_Microphones_d_vice | awk '{ print $2 }')"
fi