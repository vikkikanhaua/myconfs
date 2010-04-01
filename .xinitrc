#!/bin/bash

# {{{ Log settings
errorlog="$HOME/.xsession-errors"

# Start with a clean log file every time
if ( cp /dev/null "$errorlog" 2> /dev/null ); then
  chmod 600 "$errorlog"
  exec > "$errorlog" 2>&1
fi
# }}}

# feh --bg-scale ~/pictures/"$(ls ~/pictures | sort -R | line)"

eval `cat ~/.fehbg`

urxvtd -q -f -o &

# xsetroot -cursor_name left_ptr &

setxkbmap -option terminate:ctrl_alt_bksp &

exec awesome
