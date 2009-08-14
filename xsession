export PATH=`~/etc/build_path.py`
gconftool-2 --set /apps/panel/toplevels/top_panel_screen0/auto_hide_size 0 --type int &
gconftool-2 --set /apps/panel/toplevels/top_panel_screen0/hide_delay 0 --type int &
gconftool-2 --set /apps/panel/toplevels/top_panel_screen0/unhide_delay 0 --type int &
gconftool-2 --set /apps/panel/toplevels/top_panel_screen0/enable_animations 0 --type bool &
gnome-settings-daemon &
pypanel &
gnome-panel &
gnome-power-manager &
nm-applet &
tomboy --open-note TODO &
gnome-do &
pidgin &
skype &
exec xmonad
