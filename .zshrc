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

zstyle :compinstall filename '/home/vikki/.zshrc'

autoload -Uz compinit promptinit complist zutil
compinit
promptinit
prompt adam2
# End of lines added by compinstall
export HISTFILE=~/.zsh_history
export HISTSIZE=5000
export SAVEHIST=5000
export PATH=$PATH:/usr/local/bin
export EDITOR="vim"
eval `dircolors -b`

setopt appendhistory autocd extendedglob nomatch notify correctall hist_ignore_all_dups hist_ignore_space
setopt autopushd pushdminus pushdsilent pushdtohome
setopt nohup
bindkey -v

# My Aliases
if [ -f ~/.aliases ]; then
        . ~/.aliases
fi

# {{{ Key Bindings
bindkey '\e[A' history-beginning-search-backward
bindkey '\e[B' history-beginning-search-forward
#bindkey '^[[1~' beginning-of-line
#bindkey '^[[4~' end-of-line
bindkey "\e[3~" delete-char
bindkey '^i' expand-or-complete-prefix
bindkey "\e[8~" end-of-line
bindkey "\e[7~" beginning-of-line
 
#bindkey "\e[1~" beginning-of-line
#bindkey "\e[4~" end-of-line
#bindkey "\e[5~" beginning-of-history
#bindkey "\e[6~" end-of-history
#bindkey "\e[3~" delete-char
#bindkey "\e[2~" quoted-insert
#bindkey "\e[5C" forward-word
#bindkey "\e[5D" backward-word
#bindkey "\e\e[C" forward-word
#bindkey "\e\e[D" backward-word
#bindkey "^H" backward-delete-word
# for rxvt
bindkey "\e[8~" end-of-line
bindkey "\e[7~" beginning-of-line
# completion in the middle of a line
#bindkey '^i' expand-or-complete-prefix
# }}}

# My Functions
# Usage: extract <file>
# Description: extracts archived files (maybe)
extract () {
    if [ -f $1 ]; then
            case $1 in
            *.tar.bz2)  tar -jxvf $1        ;;
            *.tar.gz)   tar -zxvf $1        ;;
            *.bz2)      bzip2 -d $1         ;;
            *.gz)       gunzip -d $1        ;;
            *.tar)      tar -xvf $1         ;;
            *.tgz)      tar -zxvf $1        ;;
            *.zip)      unzip $1            ;;
            *.Z)        uncompress $1       ;;
            *.rar)      unrar x $1            ;;
            *)          echo "'$1' Error. Please go away" ;;
            esac
            else
            echo "'$1' is not a valid file"
  fi
  }
