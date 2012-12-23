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
zstyle ':completion:*' menu select=1 _complete _ignored _approximate
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'

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
export PATH=/home/vikki/.bin:$PATH
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
source ~/.zsh/aliases
source ~/.zsh/colors
source ~/.zsh/functions
# }}}

# ---[ keybindings ] {{{
bindkey -v
bindkey '^[Oc'   forward-word                   #for normal shell
bindkey '^[Od'   backward-word
bindkey '^[OC'   forward-word                   #for tmux
bindkey '^[OD'   backward-word
bindkey '^[[1~'  beginning-of-line              #for screen
bindkey '^[[4~'  end-of-line
bindkey '^[[7~'  beginning-of-line              #for rxvt
bindkey '^[[8~'  end-of-line
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

#---[ PROMPT ] {{{
precmd () {
  [[ $? -eq 0 ]] && color="%{[1;32m%}" || color="%{[1;31m%}"

  export PROMPT=" %{[0;34m%}%~${color} $%{[0m%} "

  [[ -n "$RANGER_LEVEL" ]] && export RPROMPT="(in ranger; level=$RANGER_LEVEL)"
}
# }}}

# ---[ login manager ] {{{
if [[ -z "$DISPLAY" ]] && [[ $(tty) = /dev/tty1 ]] { exec startx -- vt1 &> /dev/null }
# }}}
