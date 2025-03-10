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
alias gl='git log --graph --abbrev-commit --decorate --format=format:"%C(cyan)%h%C(reset) - %an - %C(bold red)(%ar)%C(reset) %C(bold cyan)%s%C(reset) %C(auto)%d%C(reset)" --all -20'
alias gs='git status'
alias gd='git diff'
alias gdt='git difftool'
alias ga='git add'
alias nvimdiff='nvim -d'

# Application overrides
alias vim=nvim

# Elm stuff
alias et='elm-test-rs --compiler `which lamdera` ./tests/*'
alias etw='elm-test-rs --watch --compiler `which lamdera` ./tests/*'
alias ep='npx elm-pages'
alias epa='npx elm-pages run AddRoute'

# Nix stuff
alias hmr='home-manager switch --flake $HOME/dotfiles/home-manager'
