#!/bin/bash

# {{{ Log settings
errorlog="$HOME/.xsession-errors"

# Start with a clean log file every time
if ( cp /dev/null "$errorlog" 2> /dev/null ); then
  chmod 600 "$errorlog"
  exec > "$errorlog" 2>&1
fi
# }}}

urxvtd -q -f -o &

setxkbmap -option terminate:ctrl_alt_bksp &

eval `cat ~/.fehbg`

# xsetroot -bg "#121212" -mod 3 3

exec awesome
