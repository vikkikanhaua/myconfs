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

autoload -Uz compinit complist zutil promptinit
promptinit
compinit
prompt adam2
export HISTFILE=~/.zsh_history
export HISTSIZE=5000
export SAVEHIST=5000
export EDITOR="vim"
export PATH=$PATH:/usr/local/bin/
eval `dircolors -b`

setopt autocd extendedglob nomatch notify correctall hist_ignore_all_dups hist_ignore_space
setopt autopushd pushdminus pushdsilent pushdtohome
setopt INC_APPEND_HISTORY
setopt nohup

# My Aliases
if [ -f ~/.aliases ]; then
        . ~/.aliases
fi

# make screen's title change right {{{
if [[ $TERM =~ "screen" ]]
then
  set-title ()
  {
		if [[ $* == "zsh" ]]
		then
			builtin echo -ne "\ek\$$*\e\\"
		else
			builtin echo -ne "\ek$*\e\\"
		fi
	}

	set-hardstatus ()
	{
		if [[ $* == "zsh" ]]
		then
			printf '\e_%s\e\' $VIMODE
		else
			printf '\e_\e\'
		fi
  }
 
  preexec () 
  {
    if [[ -n $STY ]] 
    then
        TITLE=${$(echo $3 | sed -r 's/^sudo ([^ ]*) .*/#\1/;tx;s/^([^ ]*) +.*/\1/;s/^([^ ]*)$/$\1/;:x;q')/#*\/} 
				set-title $TITLE
				set-hardstatus $TITLE
    fi
  }

  precmd () 
  { 
    if [[ -n $STY ]] 
    then
      TITLE=${0/#*\/} 
			set-title $TITLE
			set-hardstatus $TITLE
    fi
  }

	set-mode () { 
		VIMODE='--'$1'--'
		set-hardstatus $TITLE
	}

	bindkey -v
	set-mode INSERT

fi
#}}}

# {{{ Key Bindings
# for rxvt
bindkey -v
#for screen
bindkey '^[[1~'  beginning-of-line
bindkey "^[[4~"  end-of-line
#for rxvt
bindkey "^[[8~"  end-of-line
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

# My Functions
# Usage: extract <file>
# Description: extracts archived files (maybe)
extract () {
    if [ -f $1 ]; then
            case $1 in
            *.tar.bz2)  tar jxvf $1        ;;
            *.tar.gz)   tar zxvf $1        ;;
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

#grey="%{$(echo -n '\e[1;30m')%}"
#red="%{$(echo -n '\e[1;31m')%}"
#green="%{$(echo -n '\e[1;32m')%}"
#yellow="%{$(echo -n '\e[1;33m')%}"
#blue="%{$(echo -n '\e[1;34m')%}"
#magenta="%{$(echo -n '\e[1;35m')%}"
#cyan="%{$(echo -n '\e[1;36m')%}"
#white="%{$(echo -n '\e[1;37m')%}"
#lored="%{$(echo -n '\e[0;31m')%}"
#logreen="%{$(echo -n '\e[0;32m')%}"
#loyellow="%{$(echo -n '\e[0;33m')%}"
#loblue="%{$(echo -n '\e[0;34m')%}"
#lomagenta="%{$(echo -n '\e[0;34m')%}"
#locyan="%{$(echo -n '\e[0;35m')%}"
#lowhite="%{$(echo -n '\e[0;37m')%}"
