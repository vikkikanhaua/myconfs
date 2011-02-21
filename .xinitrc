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

tmux start-server

eval `cat ${HOME}/.fehbg`

xsetroot -cursor_name left_ptr

nvidia-settings --load-config-only

setxkbmap -option terminate:ctrl_alt_bksp

dzonky

exec ck-launch-session xmonad
