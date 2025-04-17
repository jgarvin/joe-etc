#!/usr/bin/env bash

set -e
set -o pipefail

cd ~/.fonts

if [ -f .downloaded_fonts ]; then
    exit 0
fi

curl -s 'https://api.github.com/repos/be5invis/Iosevka/releases/latest' | jq -r ".assets[] | .browser_download_url" | grep PkgTTC-Iosevka | xargs -n 1 curl -L -O --fail --silent --show-error

for i in *.zip; do
    unzip -o $i
done

rm *.zip

touch .downloaded_fonts
