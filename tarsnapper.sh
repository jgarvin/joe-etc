#!/bin/bash

set -x

DELTAS="1d 7d 31d"

source ~/opt/tarsnapper/bin/activate
tarsnapper --target etc-\$date --sources ~/etc/  --deltas $DELTAS - make
tarsnapper --target ssc-\$date --sources ~/ssc/  --deltas $DELTAS - make
tarsnapper --target oldssc-\$date --sources ~/oldssc/  --deltas $DELTAS - make

