#!/usr/bin/env bash

set -x

DELTAS="1d 7d 31d"

source ~/opt/tarsnapper/bin/activate
tarsnapper --target etc-\$date --sources ~/etc/  --deltas $DELTAS - make
tarsnapper --target ssc-\$date --sources ~/ssc/  --deltas $DELTAS - make
tarsnapper --target oldssc-\$date --sources ~/oldssc/  --deltas $DELTAS - make
tarsnapper --target talon-settings-\$date --sources ~/.talon/user/  --deltas $DELTAS - make
tarsnapper --target obsidian-config-\$date --sources ~/.config/obsidian/  --deltas $DELTAS - make
tarsnapper --target obsidian-data-\$date --sources ~/Documents/uber/  --deltas $DELTAS - make
