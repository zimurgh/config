#-------------------------------------------#
# File:	    zshrc	                    #
# Version:  0.0.5 			    #
# Author:   Michael Carpenter		    #
# Date:	    14/8/15                         #
#-------------------------------------------#
#ZSH=/usr/share/oh-my-zsh/
#DISABLE_AUTO_UPDATE="true"
#ZSH_CACHE_DIR=$HOME/.oh-my-zsh.cache
#if [[ ! -d $ZSH_CACHE_DIR ]];
#then
#    mkdir $ZSH_CACHE_DIR
#fi

#source $ZSH/oh-my-zsh.sh

autoload -Uz compinit
autoload colors; colors;
compinit

#ZSH_THEME="powerline"

# History
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

# Variables
color='red'
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/home/oldmanmike/bin:/home/oldmanmike/.cabal/bin:/home/oldmanmike/.local/bin
export BROWSER="firefox"
export EDITOR="vim"
export PDF="zathura"

# Alias
setopt completealiases
#alias grep='grep --color=auto'
#alias ls='ls --color=auto'
alias scan='sudo ip link set wlp3s0 up && sudo iw dev wlp3s0 scan | less'
alias keyboard='setxkbmap -layout us -variant dvp -option compose:102 -option numpad:shift3 -option kpdl:semi -option keypad:atm -option caps:shift && setxkbmap -option ctrl:swapcaps'
alias refresh='source ~/.zshrc'

# Completion
zmodload zsh/complist
autoload -Uz compinit && compinit
zstyle :compinstall filename '${HOME}/.zshrc'

# Prompt
setprompt () {
autoload -U colors promptinit && colors && promptinit
PS1="%{$fg[red]%}[%{$fg[cyan]%}%D %T%{$fg[red]%}][%{$fg[green]%}%n%{$reset_color%}@%{$fg[green]%}%m%{$fg[red]%}]%{$reset_color%}
%{$fg[red]%}[%{$fg[yellow]%}%~%{$fg[red]%}][%{$reset_color%}%#%{$fg[red]%}>%{$reset_color%} "
PS2="%{$fg[red]%}>>%{$reset_color%} "
}
setprompt

# Execution

# Stuff
setopt extendedglob
unsetopt beep
bindkey -v
