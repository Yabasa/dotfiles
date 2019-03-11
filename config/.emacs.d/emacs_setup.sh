#!/bin/sh
# Simple validation to make sure script is running within git repo and that the essentail dotfiles are present
this_script_path=$( cd "$(dirname "$0")" ; pwd )
if [ $( basename $this_script_path ) = "emacs" ] && [ -f $this_script_path/init.el ] && [ -f $this_script_path/org_mode_config.el ] && [ -d $this_script_path/my_mods ]; then
    echo "found init.el and all other requried dotfiles in repo, lets continue"
else
    echo "something is wrong, either this script is not running from within it's original git repo or the init.el or some other dotfile is missing from the git repo"
    echo "stopping here..."
    exit 1
fi

mkdir -p ~/.emacs.d

# Point ~/init.el to the version in this repo
# Create backup if one already exists and create soft link to this one
if [ -L ~/.emacs.d/init.el ]; then
    echo "the current init.el is a symlink, remapping symlink to point to this repo"
    mv ~/.emacs.d/init.el ~/.emacs.d/init.el.tmp
    ln -s $this_script_path/init.el ~/.emacs.d/init.el
    if [ $? -eq 0 ]; then
        rm ~/.emacs.d/init.el.tmp
    else
        echo "something went wrong trying to re-create the init.el symlink, leaving the existing one as it is"
        mv ~/.emacs.d/init.el.tmp ~/.emacs.d/init.el
    fi
elif [ -f ~/.emacs.d/init.el ]; then
    echo "init.el already exists, making a backup before installing new one"
    backed_up_name=init.el_$(date +"%Y%m%d_%H%M%S")
    mv ~/.emacs.d/init.el ~/.emacs.d/$backed_up_name
    echo "old init.el now called "$backed_up_name
    ln -s $this_script_path/init.el ~/.emacs.d/init.el
    echo "installed new init.el by making symlink to init.el in this repo"
else
    echo "no existing init.el found in ~/.emacd.s/ directory, installing this one by making a symlink to init.el in this repo"
    ln -s $this_script_path/init.el ~/.emacs.d/init.el
fi

# Point ~/org_mode_config.el to the version in this repo
# Create backup if one already exists and create soft link to this one
if [ -L ~/.emacs.d/org_mode_config.el ]; then
    echo "the current org_mode_config.el is a symlink, remapping symlink to point to this repo"
    mv ~/.emacs.d/org_mode_config.el ~/.emacs.d/org_mode_config.el.tmp
    ln -s $this_script_path/org_mode_config.el ~/.emacs.d/org_mode_config.el
    if [ $? -eq 0 ]; then
        rm ~/.emacs.d/org_mode_config.el.tmp
    else
        echo "something went wrong trying to re-create the org_mode_config.el symlink, leaving the existing one as it is"
        mv ~/.emacs.d/org_mode_config.el.tmp ~/.emacs.d/org_mode_config.el
    fi
elif [ -f ~/.emacs.d/org_mode_config.el ]; then
    echo "org_mode_config.el already exists, making a backup before installing new one"
    backed_up_name=org_mode_config.el_$(date +"%Y%m%d_%H%M%S")
    mv ~/.emacs.d/org_mode_config.el ~/.emacs.d/$backed_up_name
    echo "old org_mode_config.el now called "$backed_up_name
    ln -s $this_script_path/org_mode_config.el ~/.emacs.d/org_mode_config.el
    echo "installed new org_mode_config.el by making symlink to org_mode_config.el in this repo"
else
    echo "no existing org_mode_config.el found in ~/.emacd.s/ directory, installing this one by making a symlink to org_mode_config.el in this repo"
    ln -s $this_script_path/org_mode_config.el ~/.emacs.d/org_mode_config.el
fi
