#!/bin/bash
 
# {{{ Log settings
errorlog="$HOME/.xsession-errors"

# Start with a clean log file every time
if ( cp /dev/null "$errorlog" 2> /dev/null ); then
  chmod 600 "$errorlog"
  exec > "$errorlog" 2>&1
fi
# }}}

export OOO_FORCE_DESKTOP="gnome" &

#Esetroot -s ~/Pictures/`ls ~/Pictures | sort -R | line` &

Esetroot -s ~/Pictures/skeleton_wallpaper_by_icoeph.jpg &

~/.bin/dzonky &

xsetroot -cursor_name left_ptr &

setxkbmap -option terminate:ctrl_alt_bksp &

xscreensaver -no-splash & 

exec ~/.xmonad/xmonad-x86_64-linux
