#!/bin/bash

# {{{ Log settings
errorlog="$HOME/.xsession-errors"

# Start with a clean log file every time
if ( cp /dev/null "$errorlog" 2> /dev/null ); then
  chmod 600 "$errorlog"
  exec > "$errorlog" 2>&1
fi
# }}}

# eval `cat ~/.fehbg`

urxvtd -q -f -o &

remuco-mpd &

# xsetroot -cursor_name left_ptr &

setxkbmap -option terminate:ctrl_alt_bksp &

feh --bg-scale "$HOME/pictures/$(ls $HOME/pictures | sort -R | line)"

exec awesome
