# {{{ Completion
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
zstyle ':completion:*:descriptions' format "%B---- %d%b"
 
# Messages/warnings format
zstyle ':completion:*:messages' format '%B%U---- %d%u%b'
zstyle ':completion:*:warnings' format '%B%U---- no match for: %d%u%b'
 
# Describe options in full
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
# }}}

autoload -Uz compinit complist zutil
compinit

# exports {{{
export HISTFILE=~/.zsh_history
export CC="colorgcc"
export HISTSIZE=5000
export SAVEHIST=5000
export EDITOR="vim"
export PATH=$PATH:~/.bin
#export MAIL=""
# }}}

setopt autocd extendedglob nomatch notify correctall hist_ignore_all_dups hist_ignore_space
setopt autopushd pushdminus pushdsilent pushdtohome
setopt INC_APPEND_HISTORY
setopt nohup

# sourced files {{{
# My Aliases
if [ -f ~/.aliases ]; then
        source ~/.aliases
fi

# My Functions
if [ -f ~/.funcs ]; then
        source ~/.funcs
fi
# }}}

if [[ $TERM =~ "screen" ]]; then
  set-title () {
    if [[ $* == "zsh" ]]; then
      builtin echo -ne "\ek\$$*\e\\"
    else
      builtin echo -ne "\ek$*\e\\"
    fi
  }
 
  preexec () {
    if [[ -n $STY ]]; then
      TITLE=${$(echo $3 | sed -r 's/^sudo ([^ ]*) .*/#\1/;tx;s/^([^ ]*) +.*/\1/;s/^([^ ]*)$/$\1/;:x;q')/#*\/} 
      set-title $TITLE
    fi
  }

  precmd () { 
    if [[ -n $STY ]]; then
      TITLE=${0/#*\/} 
      set-title $TITLE
    fi
  }
fi

# keybindings {{{
bindkey -v
#for screen
bindkey '^[[1~'  beginning-of-line
bindkey '^[[4~'  end-of-line
#for rxvt
bindkey '^[[8~'  end-of-line
bindkey '^[[7~'  beginning-of-line
bindkey '^[[3~'  delete-char
bindkey '^[[5~'  .undefined-key
bindkey '^[[6~'  .undefined-key
bindkey '^[[2~'  overwrite-mode
bindkey '^?'     backward-delete-char
bindkey '^['     vi-cmd-mode
bindkey '^r'     .history-incremental-pattern-search-backward
bindkey '^b'     clear-screen
bindkey '^k'     kill-whole-line
bindkey '^y'     .vi-yank-whole-line
bindkey '^[Oc'   forward-word
bindkey '^[Od'   backward-word
# }}}

# prompt {{{ 
color="$(echo -n '\e[0;35m')"
rst="$(echo -n '\e[0m')"
export PS1="$rst┌─[$color %~ $rst]
└─╼ "
# }}}

# login manager {{{
#
if [[ -z "$DISPLAY" ]] && [[ $(tty) = /dev/tty1 ]] { exec startx }
#
# }}}
