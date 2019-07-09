# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH=/usr/local/bin:/usr/local/sbin:~/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="refined"

plugins=(git tmux colorize sudo zsh-autosuggestions rsync)

source $ZSH/oh-my-zsh.sh

# User configuration

export VIRTUAL_ENV_DISABLE_PROMPT=

# Preferred editor for local and remote sessions
export EDITOR='nvim'

# ssh
export SSH_KEY_PATH="~/.ssh/id_rsa"

# x forwarding tmux
echo $DISPLAY > ~/.display.txt
alias up_disp='export DISPLAY=`cat ~/.display.txt`'

# set colors
export CLICOLOR=1

# edit shell commands in vim
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^x^x' edit-command-line
