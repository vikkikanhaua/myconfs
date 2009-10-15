#!/bin/bash
 
#Esetroot -s ~/Pictures/`ls ~/Pictures | sort -R | line` &

Esetroot -s ~/Pictures/mypattern.png

~/.bin/dzonky &

xsetroot -cursor_name left_ptr &

xscreensaver -no-splash &

exec ~/.xmonad/xmonad-x86_64-linux
