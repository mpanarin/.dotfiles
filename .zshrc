# Path to oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Name of the theme to load.
ZSH_THEME="spaceship"

# Display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

plugins=(git python vi-mode django docker extract lol mix pip elixir poetry kubectl asdf)

source $ZSH/oh-my-zsh.sh

# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -z $SSH_CONNECTION ]]; then
  export EDITOR='nvim'
else
  export EDITOR='vi'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"

# Do not start tmux if we are inside Emacs
if [[ -z $INSIDE_EMACS ]]; then
  # TMUX startup
  source ~/.gem/ruby/2.6.0/gems/tmuxinator-0.15.0/completion/tmuxinator.zsh
  ZSH_TMUX_AUTOSTART=false
  eval $(~/tmux_get_startup_command)
  powerline-config tmux setup
fi

# Spaceship theme customization
SPACESHIP_USER_SHOW=false
SPACESHIP_HG_SHOW=false
SPACESHIP_HG_STATUS_SHOW=false
SPACESHIP_PACKAGE_SHOW=false
SPACESHIP_NODE_SHOW=false
SPACESHIP_RUBY_SHOW=false
SPACESHIP_XCODE_SHOW_LOCAL=false
SPACESHIP_XCODE_SHOW_GLOBAL=false
SPACESHIP_SWIFT_SHOW_LOCAL=false
SPACESHIP_GOLANG_SHOW=false
SPACESHIP_PHP_SHOW=false
SPACESHIP_RUST_SHOW=false
SPACESHIP_HASKELL_SHOW=false
SPACESHIP_JULIA_SHOW=false
SPACESHIP_AWS_SHOW=false
SPACESHIP_DOTNET_SHOW=false
SPACESHIP_EMBER_SHOW=false
SPACESHIP_KUBECONTEXT_SHOW=false
SPACESHIP_BATTERY_SHOW=false
SPACESHIP_DOCKER_SHOW=false

# Enable zsh autosuggestions
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_USE_ASYNC=1
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=30
ZSH_AUTOSUGGEST_STRATEGY=match_prev_cmd
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=0'
bindkey '^ ' autosuggest-accept

# Enable zsh syntax highlight
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# VIRTUALENV WRAPPER STUFFS
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python
source /usr/bin/virtualenvwrapper.sh

# Enable fzf
export FZF_TMUX=1
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# added by pipsi (https://github.com/mitsuhiko/pipsi)
export PATH="/home/m-panarin/.local/bin:$PATH"

# Poetry
export PATH="$PATH:/home/$USER/.poetry/bin"

# Python startup
export PYTHONSTARTUP="$(python -m jedi repl)"

# asdf-vm configs
. $HOME/.asdf/asdf.sh
. $HOME/.asdf/completions/asdf.bash

# disable automatic cd in zsh
unsetopt AUTO_CD

# Aliases
alias gdt='git difftool'

alias estat='systemctl status --user emacs.service'
alias estart='systemctl start --user emacs.service'
alias estop='systemctl stop --user emacs.service'
alias elog='journalctl -u emacs.service --since today'

alias doco=docker-compose
alias doco_rebuild='doco down -v && doco up --build'
alias doco_log='docker-compose logs'

function omae_wa_mou_shindeiru() {
    echo 'NANI?!'
    sleep 1
    if [ -z "$1" ]
    then
        systemctl poweroff -i
    else
        pkill $1
    fi
}

function ranger-cd {
    tempfile="$(mktemp -t tmp.XXXXXX)"
    ranger --choosedir="$tempfile" "${@:-$(pwd)}"
    test -f "$tempfile" &&
        if [ "$(cat -- "$tempfile")" != "$(echo -n `pwd`)" ]; then
            cd -- "$(cat "$tempfile")"
        fi
    rm -f -- "$tempfile"
}

bindkey -s '^o' 'ranger-cd\n'

function tnew() {
    if [ -z "$1" ]
    then
        name=""
    else
        name=" -s $1"
    fi
    tmux detach-client -t /dev/pts/1 -E "tmux new $name"
}

function tatt() {
    if [ -z "$1" ]
    then
        name=""
    else
        name=" -t $1"
    fi
    tmux detach-client -t /dev/pts/1 -E "tmux attach $name"
}

alias vim='nvim'

alias gsubsi='g submodule init && g submodule sync && g submodule update'
alias gsubi='g submodule update --init'

alias xa='exa -lh --git'
alias xat='exa -lTh --git'

alias b='bat'
alias cat='bat'

alias ezsh='nvim ~/.zshrc && source ~/.zshrc'
alias tkill='tmux kill-session -t'

alias kub='kubectl'

source /home/m-panarin/.local/share/dephell/_dephell_zsh_autocomplete
