#!/bin/bash

cd

# add emacs PPA
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt-get update

# install emacs pip zsh gem, curl and vim
sudo apt-get install -y emacs25 python-pip zsh ruby curl vim flake8 silversearcher-ag
pip install --upgrade pip
sudo apt-get upgrade -y

# Install spacemacs
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

# symlink the spacemacs config
ln -fs .dotfiles/.spacemacs .spacemacs

#install fonts
bash .dotfiles/install_fonts.sh

# install tmux and dependencies
sudo apt install -y automake build-essential pkg-config libevent-dev libncurses5-dev
git clone https://github.com/tmux/tmux.git /tmp/tmux
cd /tmp/tmux
bash autogen.sh
./configure && make
sudo make install
cd -
rm -rf /tmp/tmux

# powerline for tmux and shell, tmuxinator
pip install --user powerline-status
ln -fs .dotfiles/.tmux.conf .tmux.conf
# TODO: Ruby version need to be specified, potentially broken
sudo gem install tmuxinator -v 0.10.1

# make zsh default shell
chsh -s $(which zsh)

# install oh-my-zsh
curl -fsSL https://raw.githubusercontent.com/PanarinM/oh-my-zsh/install-silent/tools/install_silent.sh > install.sh
sh install.sh
rm install.sh

# install spaceship theme
curl -o - https://raw.githubusercontent.com/denysdovhan/spaceship-zsh-theme/master/install.zsh | zsh

# symlink zshrc
cd
ln -fs .dotfiles/.zshrc .zshrc

# install virtualenv
sudo pip install virtualenv

# fix anaconda-mode sluggishness
sudo pip install --upgrade "jedi>=0.9.0" "json-rpc>=1.8.1" "service_factory>=0.1.5"

# add solarized theme for vim
mkdir -p ~/.vim/pack/themes/opt
git clone https://github.com/lifepillar/vim-solarized8.git ~/.vim/pack/themes/opt

cd
# set sources for configs
bash ./dotfiles/set_sources.sh
ln -fs .dotfiles/.gitconfig .gitconfig
ln -fs .dotfiles/.bashrc .bashrc
ln -fs .dotfiles/.pylintrc .pylintrc
ln -fs .dotfiles/.vimrc .vimrc

sudo reboot now
