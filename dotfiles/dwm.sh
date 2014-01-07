#!/bin/sh
xsetroot -bg black
xrdb -merge ~/.Xresources &
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

xset m 20/4 1
xset r rate 200 50

xmodmap -e 'keycode 166 = NoSymbol'
xmodmap -e 'keycode 167 = NoSymbol'

(while true
do
        xset s 60 +dpms s blank s expose        
	sleep 70
done) & 
source /etc/X11/xinit/xinitrc.d/30-dbus
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
export GNOME_KEYRING_CONTROL GNOME_KEYRING_PID GPG_AGENT_INFO SSH_AUTH_SOCK

xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Axes" 6 7 4 5
xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 1
xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 2
xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Timeout" 200
xdg-mime default chromium.desktop x-scheme-handler/https
xdg-mime default chromium.desktop x-scheme-handler/http

xrandr --output DP3 --off --output DP2 --off --output DP1 --off --output HDMI2 --off --output HDMI1 --off --output LVDS1 --mode 1280x800 --pos 328x1080 --rotate normal --output VGA1 --mode 1920x1080 --pos 0x0 --rotate normal
emacs &
xss slock &
xset s 600

dwm 
