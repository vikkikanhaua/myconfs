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

urxvtd -q -f -o &

Esetroot -s ~/Pictures/"$(ls ~/Pictures | sort -R | line)" &

~/.bin/dzonky &

xsetroot -cursor_name left_ptr &

setxkbmap -option terminate:ctrl_alt_bksp &

exec ~/.xmonad/xmonad-x86_64-linux
