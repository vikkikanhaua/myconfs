#!/bin/bash
 
mplayer -fs ~/dwhelper/Rio6000-ArchlinuxAnimationWithSound207.mov

Esetroot -s `perl -e '@images = glob "~/Pictures/*"; print $images[int rand $#images+1], "\n"'` &

alsactl restore &

~/.dzonky &

xsetroot -cursor_name left_ptr &

#xcompmgr -ncC &

xscreensaver -no-splash &

#if [ -x /usr/bin/gnome-power-manager ] ; then
#   sleep 5
#   gnome-power-manager
#fi

exec ~/.xmonad/xmonad-x86_64-linux
