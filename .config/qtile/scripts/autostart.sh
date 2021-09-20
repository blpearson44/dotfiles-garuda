#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

#feh --bg-fill ~/.config/qtile/flower.jpg &
#conky -c ~/.config/conky/conky.conf &

# xrandr --output DP-0 --primary --mode 2560x1440 --rate 144.0 --output HDMI-0  --mode 2560x1440 --rate 74.99 --left-of DP-0

#starting utility applications at boot time
lxsession &
run nm-applet &
run pamac-tray &
numlockx on &
blueman-applet &
albert &
#flameshot &
#picom --config $HOME/.config/picom/picom.conf &
picom --config .config/picom/picom-blur.conf --experimental-backends &
#/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
dunst &
feh --bg-fill ~/Pictures/wallpapers/dark/cuts-of-life.png ~/Pictures/wallpapers/dark/josie-iv--2560×1440.png
#starting user applications at boot time
run volumeicon &
run cbatticon &
#run discord &
#nitrogen --random --set-zoom-fill &
#run caffeine -a &
#run vivaldi-stable &
#run firefox &
#run thunar &
#run dropbox &
#run insync start &
#run spotify &
#run atom &
#run telegram-desktop &
