# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH=/usr/local/bin:/usr/local/sbin:~/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=/Users/hariank/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git tmux colorize zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
export EDITOR='nvim'

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/id_rsa"

# prompt
export PROMPT='[%{$fg[red]%}%n:%{$fg[white]%}%c%{$reset_color%}] '

# alias
alias zshconfig="vim ~/.zshrc"
alias ohmyzsh="vim ~/.oh-my-zsh"
alias proj='cd ~/Documents/coding/Projects/'
alias amount='~/amount'
alias andrew='~/andrew'
alias g++11="/usr/local/bin/g++-4.9 -std=c++11"
alias vim="/Applications/MacVim.app/Contents/MacOS/Vim"

# set colors
export CLICOLOR=1

# other
export GOPATH=$HOME/work
. /Users/hariank/torch/install/bin/torch-activate
export LD_LIBRARY_PATH=~/torch/install/lib:$LD_LIBRARY_PATH

# edit shell commands in vim
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^x^x' edit-command-line
