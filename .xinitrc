#!/bin/bash

# {{{ Log settings
errorlog="$HOME/.xsession-errors"

# Start with a clean log file every time
if ( cp /dev/null "$errorlog" 2> /dev/null ); then
  chmod 600 "$errorlog"
  exec > "$errorlog" 2>&1
fi
# }}}

urxvtd -q -f -o

setxkbmap -option terminate:ctrl_alt_bksp

nvidia-settings --load-config-only

xsetroot -cursor_name left_ptr

conky -c ~/.conky/conkyrc

eval `cat ${HOME}/.fehbg`

# ranwall

exec xmonad
