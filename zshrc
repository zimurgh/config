#-------------------------------------------#
# File:     zshrc                           #
# Version:  0.0.5                           #
# Author:   Michael Carpenter               #
# Date:     14/8/15                         #
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
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/home/oldmanmike/bin:/home/oldmanmike/.cabal/bin:/home/oldmanmike/.local/bin:/home/oldmanmike/.gem/ruby/2.2.0/bin
export BROWSER="firefox"
export EDITOR="vim"
export PDF="zathura"
export GOPATH=$HOME
# Alias
setopt completealiases
#alias grep='grep --color=auto'
#alias ls='ls --color=auto'
alias scan='sudo ip link set wlp3s0 up && sudo iw dev wlp3s0 scan | less'
alias keyboard='setxkbmap -layout us -variant dvp -option compose:102 -option numpad:shift3 -option kpdl:semi -option keypad:atm -option caps:shift && setxkbmap -option ctrl:swapcaps'
alias refresh='source ~/.zshrc'
alias dev='cd ~/src/github.com/oldmanmike'
alias github='cd ~/src/github.com'
alias :r='stack build'

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

function haskell_info() {
    cabal_files=(*.cabal(N))
    if [ $#cabal_files -gt 0 ]; then
        if [ -f cabal.sandbox.config ]; then
            cabal_sandbox_info
        elif [ -f stack.yaml ]; then
            stack_info
        else
            echo "%{$fg[red]%}no stack/sandbox%{$reset_color%}"
        fi
    fi
}

function stack_info() {
    ghc_version=`ghc --version | rev | cut -d' ' -f 1 | rev`
    resolver_yaml=`cat stack.yaml | grep resolver | cut -d' ' -f 2`
    if [ -d ".stack-work/install/x86_64-linux/$resolver_yaml" ]; then
        ghc_stack=`ls .stack-work/install/x86_64-linux/$resolver_yaml/ | grep $ghc_version`
        if [ $ghc_stack ]; then
            echo "[%{$fg[green]%}$resolver_yaml %{$reset_color%}| %{$fg[green]%}$ghc_version%{$reset_color%}]"
        else
            ghc_stack=`ls .stack-work/install/x86_64-linux/$resolver_yaml/ | xargs`
            echo  "[%{$fg[green]%}$resolver_yaml%{$reset_color%}|%{$fg[red]%}$ghc_stack%{$reset_color%}]"
        fi
    else
        echo  "%{$fg[red]%}[$resolver_yaml missing please do a 'stack build']%{$reset_color%}"
    fi
}

function cabal_sandbox_info() {
    ghc_version=`ghc --version | rev | cut -d' ' -f 1 | rev`
    ghc_loc=`ls .cabal-sandbox | grep ghc | cut -d'-' -f 4 | grep $ghc_version`
    if [ $ghc_loc ]; then
        echo "[%{$fg[green]%}$ghc_version%{$reset_color%}]"
    else
        ghc_version=`ls .cabal-sandbox | grep ghc | cut -d'-' -f 4 | xargs`
        echo  "[%{$fg[red]%}$ghc_version%{$reset_color%}]"
    fi
}

RPROMPT=$(haskell_info)

# added by travis gem
[ -f /home/oldmanmike/.travis/travis.sh ] && source /home/oldmanmike/.travis/travis.sh
