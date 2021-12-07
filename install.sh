#       _____         /\  \                       /\__\                                   /\__\         /\__\    
#      /::\  \       /::\  \         ___         /:/ _/_       ___                       /:/ _/_       /:/ _/_   
#     /:/\:\  \     /:/\:\  \       /\__\       /:/ /\__\     /\__\                     /:/ /\__\     /:/ /\  \  
#    /:/  \:\__\   /:/  \:\  \     /:/  /      /:/ /:/  /    /:/__/      ___     ___   /:/ /:/ _/_   /:/ /::\  \ 
#   /:/__/ \:|__| /:/__/ \:\__\   /:/__/      /:/_/:/  /    /::\  \     /\  \   /\__\ /:/_/:/ /\__\ /:/_/:/\:\__\
#   \:\  \ /:/  / \:\  \ /:/  /  /::\  \      \:\/:/  /     \/\:\  \__  \:\  \ /:/  / \:\/:/ /:/  / \:\/:/ /:/  /
#    \:\  /:/  /   \:\  /:/  /  /:/\:\  \      \::/__/       ~~\:\/\__\  \:\  /:/  /   \::/_/:/  /   \::/ /:/  / 
#     \:\/:/  /     \:\/:/  /   \/__\:\  \      \:\  \          \::/  /   \:\/:/  /     \:\/:/  /     \/_/:/  /  
#      \::/  /       \::/  /         \:\__\      \:\__\         /:/  /     \::/  /       \::/  /        /:/  /   
#       \/__/         \/__/           \/__/       \/__/         \/__/       \/__/         \/__/         \/__/    

# ascii art generated at http://patorjk.com/software/taag


# Regular stow for simple symlinks
stow -d $HOME/dotfiles git
stow -d $HOME/dotfiles bash
if [ $( cat /proc/version | grep -c CYGWIN ) -eq 1 ]; then
    stow -d $HOME/dotfiles mintty
else
    stow -d $HOME/dotfiles kitty
fi

# --no-folding so that only files are symlinked, not dirs. This is so that if
# new files are added after installing packages they don't show up in the
# dotfiles repo
stow --no-folding -d $HOME/dotfiles nvim
stow --no-folding -d $HOME/dotfiles emacs
