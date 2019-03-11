# ~/.bashrc: executed by bash(1) for non-login shells.

# First source any private setup if found. Useful for not storing sensitive information on GitHub.
if [ -e ~/.privrc ]; then
    source ~/.privrc
fi

# Don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth

# Make sure the standard bin dirs are in the PATH
export PATH="$HOME/bin:$HOME/.local/bin:$PATH"

# Show git branch in command prompt
if [ ! $(type -t __git_ps1) ]; then
    if [ -e ~/bin/git-prompt.sh ]; then
        dummy=1
    else
        mkdir -p ~/bin
        curl -Sso ~/bin/git-prompt.sh https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
    fi
    source ~/bin/git-prompt.sh
fi
export PS1="\[\e[32m\]\u@\h\[\e[34m\]\w\[\e[31m\]\$(__git_ps1 \"(%s)\")\[\e[00m\]$ "

# LS_COLORS currently optimised for solarized color scheme
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=34:ow=34'

# *nix system specific setup
if [ $( cat /proc/version | grep -c Microsoft ) -eq 1 ]; then
    export win_home=/mnt/c/Users/trebo
    alias wh='cd $win_home'
    export org_home=$win_home/Dropbox/organiser
elif [ $( cat /proc/version | grep -c CYGWIN ) -eq 1 ]; then
    export win_home=/cygdrive/u
    alias wh='cd $win_home'
    export org_home=$win_home/organiser
else
    export org_home=~/organiser
fi

# General bash variables
# $MY_WORK_DIR: Can change this at any point so that the mwd alias (cd $MY_WORK_DIR) will redirect to this dir.
# Only set to default value if it's not already set previously e.g. in .privrc
if [ ! $MY_WORK_DIR ]; then
    export MY_WORK_DIR=~/dotfiles
fi

# Alias definitions
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# My custom bash functions
if [ -f ~/.bash_funcs ]; then
    source ~/.bash_funcs
fi
