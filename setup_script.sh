#!/bin/bash

# make a full system upgrade
sudo pacman -Syu --noconfirm

# install all needed packages
sudo pacman -S --noconfirm python2-pip \
	zsh \
	ruby \
	curl \
	vim \
	flake8 \
	the_silver_searcher \
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
	zsh-autosuggestions \
	zsh-syntax-highlighting \
	kitty \
	glu \
	mesa \
	wxgtk2 \
	libpng \
	erlang \
	elixir \
	xsel \
	flameshot \
  otf-fira-code \
  brave \
  yapf \
  xclip

# remove yakuake as it annoys me as well as i don't use it at all.
sudo pacman -R yakuake --noconfirm

# Additional packages from AUR
yay -S emacs-git telegram-desktop rocketchat-desktop gpmdp peco slack-desktop fpp-git

# add user to docker group and enable the service
sudo systemctl enable docker
sudo systemctl start docker
sudo usermod -a -G docker $USER

# upgrade both pip versions to latest release
sudo pip install --upgrade pip
sudo pip2 install --upgrade pip

# install poetry and dephell
curl -sSL https://raw.githubusercontent.com/sdispater/poetry/master/get-poetry.py | python
pip install --user dephell

# install spacemacs
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d -b develop

# symlink the spacemacs config
ln -fs ~/.dotfiles/.spacemacs ~/.spacemacs

# install all the fonts
bash ~/.dotfiles/install_fonts.sh

# install tmp - tmux plugin manager
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# symlink tmux config
ln -fs ~/.dotfiles/.tmux.conf ~/.tmux.conf

# install tmuxinator
gem install tmuxinator -v 0.12.0
sudo ln -s ~/.gem/ruby/2.5.0/bin/tmuxinator /usr/bin/tmuxinator

# make zsh default shell
chsh -s $(which zsh)

# install oh-my-zsh
curl -fsSL https://raw.githubusercontent.com/PanarinM/oh-my-zsh/install-silent/tools/install_silent.sh > install.sh
sh install.sh
rm install.sh

# install spaceship zsh theme
git clone https://github.com/denysdovhan/spaceship-prompt.git ~/.oh-my-zsh/custom/themes/spaceship-prompt
ln -s ~/.oh-my-zsh/custom/themes/spaceship-prompt/spaceship.zsh-theme ~/.oh-my-zsh/custom/themes/spaceship.zsh-theme

# install poetry
curl -sSL https://raw.githubusercontent.com/sdispater/poetry/master/get-poetry.py | python

# symlink zshrc
ln -fs ~/.dotfiles/.zshrc ~/.zshrc

# add solarized theme for vim
mkdir -p ~/.vim/pack/themes/opt
git clone https://github.com/lifepillar/vim-solarized8.git ~/.vim/pack/themes/opt/solarized8

# add kitty config folder
mkdir -p ~/.config/kitty

# add several zsh plugins
git clone https://github.com/gusaiani/elixir-oh-my-zsh.git ~/.oh-my-zsh/custom/plugins/elixir

# set-up vim Vundle
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

# add emacs daemon service
mkdir -p ~/.config/systemd/user
ln -fs ~/.dotfiles/emacs.service ~/.config/systemd/user/emacs.service
systemctl start --user emacs.service
systemctl enable --user emacs.service

# add yapf configuration
mkdir -p ~/.config/yapf
ln -sf ~/.dotfiles/style ~/.config/yapf/style

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
ln -fs ~/.dotfiles/tmux_get_startup_command ~/tmux_get_startup_command
