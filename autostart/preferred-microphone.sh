#!/bin/bash

# directions here:
# https://makandracards.com/makandra-orga/473864-how-to-set-a-fixed-microphone-source-in-ubuntu-if-it-keeps-changing

# ~$ pactl list short sources
# 1	alsa_output.pci-0000_00_1f.3.analog-stereo.monitor	module-alsa-card.c	s16le 2ch 48000Hz	SUSPENDED
# 2	alsa_input.pci-0000_00_1f.3.analog-stereo	module-alsa-card.c	s16le 2ch 44100Hz	SUSPENDED
# 12	alsa_input.usb-DPA_Microphones_d_vice_MMA-A_17060A06002B6300-00.iec958-stereo	module-alsa-card.c	s24le 2ch 48000Hz	RUNNING
# 15	alsa_input.usb-Lenovo_ThinkPad_Thunderbolt_3_Dock_USB_Audio_000000000000-00.mono-fallback	module-alsa-card.c	s16le 1ch 48000Hz	SUSPENDED


# muting/unmuting all mics here:
# https://askubuntu.com/questions/12100/command-to-mute-and-unmute-a-microphone


if pactl list short sources | grep DPA_Microphones_d_vice; then
    # mute all microphones!
    pacmd list-sources | \
        grep -oP 'index: \d+' | \
        awk '{ print $2 }' | \
        xargs -I{} pactl set-source-mute {} true
    sleep 0.25

    bad_mic="$(pactl list short sources | grep alsa_input | grep analog-stereo | awk '{ print $2 }')"
    good_mic="$(pactl list short sources | grep DPA_Microphones_d_vice | awk '{ print $2 }')"
    pactl set-default-source "$bad_mic"

    pactl set-source-mute "$bad_mic" false
    sleep 0.25
    pactl set-source-mute "$bad_mic" true
    sleep 0.25    
    pactl set-default-source "$good_mic"
    sleep 0.25
    pactl set-source-mute "$good_mic" false
    sleep 0.25
    # to deterimne this value, open the `pavucontrol` GUI app, and
    # then each time you run this command you will see the volume in
    # the input devices tab change. This value corresponds to 153%,
    # which is the maximum you are able to set in the GUI. Using the
    # command line you can set it arbitrarily high, but I haven't
    # needed to do this.
    pacmd set-source-volume "$good_mic" 100000 # https://askubuntu.com/questions/27021/setting-microphone-input-volume-using-the-command-line
fi

    # we toggle muting b/c without this sometimes preferred mic acts
    # muted, kernel or pultseaudio bug?

    # unmute all microphones!
    # pacmd list-sources | \
    #     grep -oP 'index: \d+' | \
    #     awk '{ print $2 }' | \
    #     xargs -I{} pactl set-source-mute {} false

    # pactl set-source-mute "$good_mic" false
