#!/bin/bash

# {{{ Log settings
errorlog="$HOME/.xsession-errors"

# Start with a clean log file every time
if ( cp /dev/null "$errorlog" 2> /dev/null ); then
  chmod 600 "$errorlog"
  exec > "$errorlog" 2>&1
fi
# }}}

${HOME}/.bin/dzonky &

urxvtd -q -f -o &

setxkbmap -option terminate:ctrl_alt_bksp &

eval `cat ${HOME}/.fehbg`

# xsetroot -cursor_name left_ptr &

exec awesome
# exec xmonad
