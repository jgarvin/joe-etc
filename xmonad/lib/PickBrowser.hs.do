#!/bin/sh

# xmonad versions <= 0.9 can't handle running the browser script
# internally for some reason, so we generate a source file with
# the browser string we want and have xmonad.hs import that instead.

pick_best_script=$HOME/etc/utils/pick_best_browser
redo-ifchange $pick_best_script
preference=$($pick_best_script -n)
argument=""
if [ "chromium-browser" = "$preference" ] && [ -f ~/.ssh/proxy.pac ]; then
    argument=" --proxy-pac-url=data:application\/x-javascript-config;base64,$(base64 -w0 \/home\/$LOGNAME\/.ssh\/proxy.pac)"
fi
sed -e "s/PREFERRED_BROWSER_SCRIPT_OUTPUT/\"$preference\"/g" -e "s/PREFERRED_BROWSER_SCRIPT_ARGUMENT/\"$argument\"/g" PickBrowser.hs.in > $3
