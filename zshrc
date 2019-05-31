# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH=/usr/local/bin:/usr/local/sbin:~/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=/home/hariank/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

plugins=(git tmux colorize sudo zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

# User configuration

# dircolors
eval `dircolors /home/hariank/.dir_colors/dircolors`

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

# x forwarding tmux
echo $DISPLAY > ~/.display.txt
alias up_disp='export DISPLAY=`cat ~/.display.txt`'

# set colors
export CLICOLOR=1

# other
export LD_LIBRARY_PATH=~/torch/install/lib:$LD_LIBRARY_PATH

# edit shell commands in vim
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^x^x' edit-command-line
