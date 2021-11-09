#!/bin/bash

# make a full system upgrade
sudo pacman -Syu --noconfirm

# install all needed packages
sudo pacman -S --noconfirm python2-pip \
     ranger \
     zsh \
     ruby \
     curl \
     vim \
     flake8 \
     the_silver_searcher \
     ripgrep \
     exa \
     fzf \
     base-devel \
     yay \
     tmux \
     npm \
     python-virtualenvwrapper \
     powerline-fonts \
     powerline \
     powerline-vim \
     ruby-rdoc \
     unzip \
     docker \
     docker-compose \
     kitty \
     glu \
     mesa \
     wxgtk2 \
     libpng \
     xsel \
     flameshot \
     ttf-fira-code \
     brave-browser \
     xclip \
     bat

# remove yakuake as it annoys me as well as i don't use it at all.
sudo pacman -R yakuake --noconfirm

# Additional packages from AUR
yay -S emacs-git telegram-desktop spotify peco asdf-vm

# add user to docker group and enable the service
sudo systemctl enable docker
sudo systemctl start docker
sudo usermod -a -G docker $USER

# upgrade both pip versions to latest release
sudo pip install --upgrade pip
sudo pip2 install --upgrade pip

# setup asdf
asdf plugin-add erlang elixir

# install spacemacs
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d -b develop

# symlink the spacemacs config
ln -fs ~/.dotfiles/.spacemacs ~/.spacemacs

# install tmp - tmux plugin manager
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# symlink tmux config
ln -fs ~/.dotfiles/.tmux.conf ~/.tmux.conf

# install tmuxinator
gem install tmuxinator

# install oh-my-zsh
curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh > install.sh
sh install.sh --unattended
rm install.sh

# install spaceship zsh theme
git clone https://github.com/denysdovhan/spaceship-prompt.git $ZSH/custom/themes/spaceship-prompt
ln -s ~/.oh-my-zsh/custom/themes/spaceship-prompt/spaceship.zsh-theme $ZSH/custom/themes/spaceship.zsh-theme
# install plugins
git clone https://github.com/jeffreytse/zsh-vi-mode $ZSH/custom/plugins/zsh-vi-mode
git clone https://github.com/zsh-users/zsh-autosuggestions $ZSH/custom/plugins/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git $ZSH/custom/plugins/zsh-syntax-highlighting
git clone https://github.com/gusaiani/elixir-oh-my-zsh.git $ZSH/custom/plugins/elixir

# symlink zshrc
ln -fs ~/.dotfiles/.zshrc ~/.zshrc

# make zsh default shell
chsh -s $(which zsh)

# add solarized theme for vim
mkdir -p ~/.vim/pack/themes/opt
git clone https://github.com/lifepillar/vim-solarized8.git ~/.vim/pack/themes/opt/solarized8

# add kitty config folder
mkdir -p ~/.config/kitty

# set-up vim Vundle
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

# symlink remaining files
ln -fs ~/.dotfiles/.gitconfig ~/.gitconfig
ln -fs ~/.dotfiles/.gitignore_global ~/.gitignore_global
ln -fs ~/.dotfiles/.bashrc ~/.bashrc
ln -fs ~/.dotfiles/.pylintrc ~/.pylintrc
ln -fs ~/.dotfiles/.mypy.ini ~/.mypy.ini
ln -fs ~/.dotfiles/.pycodestyle ~/.config/pycodestyle
ln -fs ~/.dotfiles/flake8 ~/.config/flake8
ln -fs ~/.dotfiles/.vimrc ~/.vimrc
ln -fs ~/.dotfiles/konsole-profile/Custom\ Solarized.colorscheme ~/.local/share/konsole/Custom\ Solarized.colorscheme
ln -fs ~/.dotfiles/konsole-profile/custom\ dark\ pastels.colorscheme ~/.local/share/konsole/custom\ dark\ pastels.colorscheme
ln -fs ~/.dotfiles/konsole-profile/Shell.profile ~/.local/share/konsole/Shell.profile
ln -fs ~/.dotfiles/konsole-profile/Courses.profile ~/.local/share/konsole/Courses.profile
ln -fs ~/.dotfiles/konsole-profile/solarized\ courses.colorscheme ~/.local/share/konsole/solarized\ courses.colorscheme
ln -fs ~/.dotfiles/.fzf.bash ~/.fzf.bash
ln -fs ~/.dotfiles/.fzf.zsh ~/.fzf.zsh
ln -fs ~/.dotfiles/kitty.conf ~/.config/kitty/kitty.conf
ln -fs ~/.dotfiles/snazzy.conf ~/.config/kitty/snazzy.conf
ln -fs ~/.dotfiles/.ripgreprc ~/.ripgreprc
ln -fs ~/.dotfiles/tmux_get_startup_command ~/tmux_get_startup_command
