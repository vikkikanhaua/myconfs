#!/bin/bash
 
#mplayer -fs ~/dwhelper/Rio6000-ArchlinuxAnimationWithSound207.mov

Esetroot -s ~/Pictures/`ls ~/Pictures | sort -R | line` &

#/usr/lib/xscreensaver/glmatrix -root &

#alsactl restore &

~/.bin/dzonky &

xsetroot -cursor_name left_ptr &

#xcompmgr -ncC &

xscreensaver -no-splash &

exec ~/.xmonad/xmonad-x86_64-linux
