# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' file-sort name
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-suffixes true
zstyle :compinstall filename '/home/vikki/.zshrc'

autoload -Uz compinit promptinit
compinit
promptinit
prompt adam2
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
export HISTFILE=~/.zsh_history
export HISTSIZE=2000
export SAVEHIST=2000
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

#Keybindings
bindkey '^?' backward-delete-char
bindkey '^[[7~' beginning-of-line
bindkey '^[[5~' up-line-or-history
bindkey '^[[3~' delete-char
bindkey '^[[8~' end-of-line
bindkey '^[[6~' down-line-or-history
bindkey '^[[A' up-line-or-search
bindkey '^[[D' backward-char
bindkey '^[[B' down-line-or-search
bindkey '^[[C' forward-char

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
