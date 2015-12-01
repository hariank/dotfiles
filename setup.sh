#!/bin/bash

dir=~/dotfiles
backup_dir=~/dotfiles_old
files="vimrc tmux.conf"

echo "making backup dir.."
mkdir -p $backup_dir
echo "success"

cd $dir
for file in $files; do
    echo "moving existing dotfile $file to backup dir.."
    mv ~/.$file $backup_dir/
    echo "making symlink.."
    ln -s $dir/$file ~/.$file
    echo "success"
done
