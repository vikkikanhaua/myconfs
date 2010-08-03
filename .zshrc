# ---[ Completion ] {{{
# :completion:<func>:<completer>:<command>:<argument>:<tag>
# Expansion options
zstyle ':completion:*' completer _complete _prefix
zstyle ':completion::prefix-1:*' completer _complete
zstyle ':completion:incremental:*' completer _complete _correct
zstyle ':completion:predict:*' completer _complete
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# Expand partial paths
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-slashes 'yes'

# Separate matches into groups
zstyle ':completion:*:matches' group 'yes'

# Describe each match group.
zstyle ':completion:*:descriptions' format "==[ %d%b ]"

# Messages/warnings format
zstyle ':completion:*:messages' format '==[ %d%u%b ]'
zstyle ':completion:*:warnings' format '==[ no match for: %d%u%b ]'

# Describe options in full
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
# }}}

# ---[ misc options ] {{{
# General
setopt   ALWAYS_TO_END NO_BEEP CLOBBER
setopt   AUTO_CD CD_ABLE_VARS MULTIOS CORRECT_ALL

# Job Control
setopt   CHECK_JOBS NO_HUP

# History
setopt   INC_APPEND_HISTORY EXTENDED_HISTORY HIST_IGNORE_ALL_DUPS HIST_FIND_NO_DUPS
setopt   HIST_REDUCE_BLANKS HIST_SAVE_NO_DUPS HIST_IGNORE_SPACE
setopt   HIST_EXPIRE_DUPS_FIRST

# Stay compatible to sh and IFS
setopt   SH_WORD_SPLIT

setopt   notify globdots pushdtohome
setopt   longlistjobs
setopt   autoresume pushdsilent
setopt   autopushd pushdminus extendedglob rcquotes mailwarning
unsetopt BG_NICE HUP autoparamslash

autoload -Uz compinit complist zutil
compinit
# }}}

# ---[ exports ] {{{
export HISTFILE=~/.zsh_history
export BROWSER="firefox"
export EDITOR="vim"
export PAGER="less -winm"
export HISTSIZE=5000
export SAVEHIST=5000
export VISUAL=$EDITOR
export PATH=~/.bin:/usr/share/perl5/vendor_perl/auto/share/dist/Cope:$PATH
export STARDICT_DATA_DIR=${HOME}/.stardict

# for colored man pages
export LESS_TERMCAP_mb=$'\E[1;31m'
export LESS_TERMCAP_md=$'\E[1;31m'
export LESS_TERMCAP_me=$'\E[1;37m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[1;34m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[1;32m'
# }}}

# ---[ sourced files ] {{{
# My Aliases
if [ -f ~/.aliases ]; then
        source ~/.aliases
fi

# My Functions
if [ -f ~/.funcs ]; then
        source ~/.funcs
fi
# }}}

# ---[ keybindings ] {{{
bindkey -v
bindkey '^[Oc'   forward-word #for normal shell
bindkey '^[Od'   backward-word
#for screen
bindkey '^[[1~'  beginning-of-line
bindkey '^[[4~'  end-of-line
#for rxvt
bindkey '^[[8~'  end-of-line
bindkey '^[[7~'  beginning-of-line
bindkey '^[[3~'  delete-char
bindkey '^[[2~'  overwrite-mode
bindkey '^r'     .history-incremental-pattern-search-backward
bindkey '^?'     backward-delete-char
bindkey '^['     vi-cmd-mode
bindkey '^b'     clear-screen
bindkey '^k'     kill-whole-line
bindkey '^y'     .vi-yank-whole-line
bindkey '^[[5~'  .undefined-key
bindkey '^[[6~'  .undefined-key
#  }}}

#---[ misc. functions + PROMPT ] {{{

set-title () {
  builtin echo -ne "\ek$*\e\\"
}

preexec () {
  if [[ -n $STY ]]; then
    TITLE=${$(echo $3 | sed -r 's/^sudo ([^ ]*) .*/#\1/;tx;s/^([^ ]*) +.*/\1/;s/^([^ ]*)$/\1/;:x;q')/#*\/}
    set-title $TITLE
  fi
}

precmd () {
  [[ $? -eq 0 ]] && color="%{\e[38;5;28m%}" || color="%{\e[38;5;160m%}"
  export PROMPT="`builtin echo "${color}➜ %{\e[38;5;166m%}%1d%{\e[0m%}"`   "

  if [[ -n $STY ]]; then
    TITLE=${0/#*\/}
    set-title $TITLE
  fi
}
# }}}

# ---[ login manager ] {{{
if [[ -z "$DISPLAY" ]] && [[ $(tty) = /dev/tty1 ]] { exec startx &> /dev/null }
# }}}

# ---[ DIRCOLORS ] {{{
export GREP_COLORS="38;5;230:sl=38;5;240:cs=38;5;100:mt=38;5;161:fn=38;5;197:ln=38;5;212:bn=38;5;44:se=38;5;166"
export LS_COLORS="st=38;5;202:*.h=38;5;81:*.rb=38;5;192:*.c=38;5;110:*.diff=42;38:*.yml=38;5;208:*.PL=38;5;178:*.csv=38;5;136:tw=38;5;003:*.chm=38;5;144:*.bin=38;5;249:*.sms=38;5;33:*.pdf=38;5;203:*.cbz=38;5;140:*.cbr=38;5;140:*.nes=38;5;160:*.mpg=38;5;38:*.ts=38;5;39:*.sfv=38;5;191:*.m3u=38;5;172:*.txt=38;5;192:*.log=38;5;190:*.bash=38;5;173:*.swp=38;5;238:*.swo=38;5;109:*.theme=38;5;109:*.zsh=38;5;173:*.nfo=38;5;113:mi=38;5;124::or=38;5;160:ex=38;5;197:ln=target:pi=38;5;126:ow=38;5;208:di=38;5;33:*.pm=38;5;197:*.pl=38;5;107:*.sh=38;5;245:*.patch=45;37:*.tar=38;5;118:*.tar.gz=38;5;118:*.zip=38;5;11::*.rar=38;5;11:*.tgz=38;5;11:*.7z=38;5;11:*.mp3=38;5;173:*.flac=38;5;166:*.mkv=38;5;115:*.avi=38;5;114:*.wmv=38;5;113:*.jpg=38;5;66:*.jpeg=38;5;66:*.png=38;5;68:*.pacnew=38;5;33:*.torrent=38;5;1:*.srt=38;5;242"
# }}}
