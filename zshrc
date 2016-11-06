#-------------------------------------------#
# File:     zshrc                           #
# Version:  0.0.5                           #
# Author:   Michael Carpenter               #
# Date:     14/8/15                         #
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
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/home/oldmanmike/bin:/home/oldmanmike/.cabal/bin:/home/oldmanmike/.local/bin:/home/oldmanmike/.gem/ruby/2.2.0/bin:/home/oldmanmike/src/github.com/oldmanmike/opensandbox/.stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/minecraftctl:/home/oldmanmike/src/github.com/oldmanmike/kaleidoscope/.stack-work/install/x86_64-linux/lts-6.5/7.10.3/bin:/home/oldmanmike/.cargo/bin:/home/oldmanmike/.multirust/toolchains/nightly-x86_64-unknown-linux-gnu/bin

export BROWSER="firefox"
export EDITOR="vim"
export PDF="zathura"
export GOPATH=$HOME
export RUST_SRC_PATH=~/.multirust/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src

# Alias
setopt completealiases
alias scan='sudo ip link set wlp3s0 up && sudo iw dev wlp3s0 scan | less'
alias keyboard='setxkbmap -layout us -variant dvp -option compose:102 -option numpad:shift3 -option kpdl:semi -option keypad:atm -option caps:shift && setxkbmap -option ctrl:swapcaps'
alias refresh='source ~/.zshrc'
alias dev='cd ~/src/github.com/oldmanmike'
alias github='cd ~/src/github.com'
alias :r='stack build'
alias ga='git add'
alias gs='git status'
alias pull='git pull origin master'
alias push='git push origin master'
alias gd='git diff'
alias gds='git diff --stat'
alias gl='git log'
alias music='zsh -c ncmpcpp'
alias ws='wine .wine/drive_c/Program\ Files\ \(x86\)/Steam/Steam.exe'

# Completion
zmodload zsh/complist
autoload -Uz compinit && compinit
zstyle :compinstall filename '${HOME}/.zshrc'

# Prompt
setprompt () {
autoload -U colors promptinit && colors && promptinit
PS1="%{$fg[red]%}[%{$fg[cyan]%}%D %T%{$fg[red]%}][%{$fg[green]%}%n%{$reset_color%}@%{$fg[green]%}%m%{$fg[red]%}][%{$fg[yellow]%}%~%{$fg[red]%}]%{$reset_color%}
%{$fg[red]%}[%{$reset_color%}%#%{$fg[red]%}>%{$reset_color%} "
PS2="%{$fg[red]%}>>%{$reset_color%} "
}
setprompt

# Stuff
setopt extendedglob
unsetopt beep
bindkey -v

# added by travis gem
[ -f /home/oldmanmike/.travis/travis.sh ] && source /home/oldmanmike/.travis/travis.sh
