#!/bin/bash

ln -fs ~/.dotfiles/.gitignore_global ~/.gitignore_global
ln -fs ~/.dotfiles/.spacemacs ~/.spacemacs
ln -fs ~/.dotfiles/.tmux.conf ~/.tmux.conf
ln -fs ~/.dotfiles/.zshrc ~/.zshrc

ln -fs ~/.dotfiles/.gitconfig ~/.gitconfig
ln -fs ~/.dotfiles/.bashrc ~/.bashrc
ln -fs ~/.dotfiles/.pylintrc ~/.pylintrc
ln -fs ~/.dotfiles/.vimrc ~/.vimrc

bash ~/.dotfiles/set_sources.sh
