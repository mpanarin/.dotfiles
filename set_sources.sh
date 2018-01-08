#!/bin/bash

USERNAME=$(whoami)
ZSCONF=.zshrc
TMUXCONF=.tmux.conf

# set .zshrc vars
OHMYZSH="export ZSH=/home/$USERNAME/.oh-my-zsh"
SPACESHIP="source \"/home/$USERNAME/.oh-my-zsh/custom/themes/spaceship.zsh-theme\""

# add them to zshrc
echo $SPACESHIP | cat - $ZSCONF > temp && mv temp $ZSCONF
echo $OHMYZSH | cat - $ZSCONF > temp && mv temp $ZSCONF

# set .tmux.conf vars
POWERLINE="source \"/home/$USERNAME/.local/lib/python2.7/site-packages/powerline/bindings/tmux/powerline.conf\""

# add them to .tmux.conf
echo $POWERLINE | cat - $TMUXCONF > temp && mv temp $TMUXCONF
