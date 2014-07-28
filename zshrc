#-------------------------------------------#
# File:			.zshrc	ZSH resource file   #
# Version:	    0.0.4 						#
# Author:		Michael Carpenter			#
# Date:	        25/12/12				    #
#-------------------------------------------#

autoload -Uz compinit
autoload colors; colors;
compinit

# History
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

# Variables
color='red'
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/home/michael/bin:/home/michael/.cabal/bin:/home/michael/.gem/ruby/2.1.0/bin
export BROWSER="firefox"
export EDITOR="vim"
export PDF="zathura"

# Alias
setopt completealiases
#alias grep='grep --color=auto'
#alias ls='ls --color=auto'
alias scan='sudo ip link set wlp3s0 up && sudo iw dev wlp3s0 scan | less'

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
