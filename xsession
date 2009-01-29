export PATH=`~/etc/build_path.py`
gnome-settings-daemon &
gnome-panel &
nm-applet &
tomboy --open-note TODO &
exec xmonad
