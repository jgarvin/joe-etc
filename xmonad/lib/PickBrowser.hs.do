#!/bin/sh

# xmonad versions <= 0.9 can't handle running the browser script
# internally for some reason, so we generate a source file with
# the browser string we want and have xmonad.hs import that instead.

pick_best_script=$HOME/etc/utils/pick_best_browser
redo-ifchange $pick_best_script
sed 's/--preferred-browser-script-output--/"'$($pick_best_script -n)'"/g' PickBrowser.hs.in > $3
