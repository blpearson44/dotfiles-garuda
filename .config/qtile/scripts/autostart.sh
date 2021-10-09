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
picom --config $HOME/.config/picom/picom.conf &
#/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
dunst &
feh --bg-fill ~/Pictures/wallpapers/dark/cuts-of-life.png ~/Pictures/wallpapers/dark/josie-iv--2560×1440.png &
#starting user applications at boot time
run volumeicon &
run spotify &
emacs --bg-daemon
