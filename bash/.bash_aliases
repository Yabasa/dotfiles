# Navigation
alias ll='ls -altr'
alias ..='cd ..'
alias h='cd ~'
alias o='cd $org_home'
alias mwd='cd $MY_WORK_DIR'

# Colorize some commands by default
alias ls='ls --color=auto'
alias grep='grep --color=auto'

# Useful shortcuts for often used command combinations
alias psg='ps -ef | grep -v "grep \-\-color=auto" | grep '

# Git shortcuts
alias gc='git commit -v'
alias gca='git commit -v -a'
alias gl='git log --oneline --all --graph --decorate -20'
alias gs='git status'
alias gd='git diff'
alias gdt='git difftool'
alias ga='git add'
alias nvimdiff='nvim -d'

# Application overrides
alias vim=nvim
