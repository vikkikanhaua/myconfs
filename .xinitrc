#!/bin/bash

FONT="-artwiz-nu-medium-*-normal-*-11-*-*-*-*-*-iso8859-*"

# {{{ Log settings
errorlog="$HOME/.xsession-errors"

# Start with a clean log file every time
if ( cp /dev/null "$errorlog" 2> /dev/null ); then
  chmod 600 "$errorlog"
  exec > "$errorlog" 2>&1
fi
# }}}

urxvtd -q -f -o

sh ${HOME}/.fehbg

xsetroot -cursor_name left_ptr # -mod 3 3 -bg "#1a1a1a"

setxkbmap -option terminate:ctrl_alt_bksp

conky -c ~/.conky/conkyrc_dzen | dzen2 -e '' -x 600 -h 14 -fn "$FONT" -ta 'r' -bg '#1a1a1a' -fg 'white' &

numlockx

exec xmonad
