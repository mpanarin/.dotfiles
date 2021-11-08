# Path to oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Name of the theme to load.
ZSH_THEME="spaceship"

# Display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# if not Emacs - use vi-mode and start tmux
if [[ -z $INSIDE_EMACS ]]; then
    # TMUX startup
    export PATH="$PATH:/home/$USER/.gem/ruby/2.7.0/bin"
    source ~/.gem/ruby/2.7.0/gems/tmuxinator-1.1.4/completion/tmuxinator.zsh
    ZSH_TMUX_AUTOSTART=false
    eval $(~/tmux_get_startup_command)
    powerline-config tmux setup
    plugins=(git python django docker extract lol mix pip elixir poetry kubectl asdf vi-mode)
else
    plugins=(git python django docker extract lol mix pip elixir poetry kubectl asdf)
    vterm_printf(){
        if [ -n "$TMUX" ]; then
            # Tell tmux to pass the escape sequences through
            # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
            printf "\ePtmux;\e\e]%s\007\e\\" "$1"
        elif [ "${TERM%%-*}" = "screen" ]; then
            # GNU screen (screen, screen-256color, screen-256color-bce)
            printf "\eP\e]%s\007\e\\" "$1"
        else
            printf "\e]%s\e\\" "$1"
        fi
    }
fi

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

# Doom Emacs
# export PATH="$PATH:/home/$USER/.emacs.d/bin"

# Python startup
export PYTHONSTARTUP="$(python -m jedi repl)"

# asdf-vm configs
export ASDF_DIR='/usr/local/opt/asdf/libexec'
export PATH="$HOME/.asdf/shims:$PATH"

# potential fix for pasting
pasteinit() {
    OLD_SELF_INSERT=${${(s.:.)widgets[self-insert]}[2,3]}
    zle -N self-insert url-quote-magic # I wonder if you'd need `.url-quote-magic`?
}

pastefinish() {
    zle -N self-insert $OLD_SELF_INSERT
}
zstyle :bracketed-paste-magic paste-init pasteinit
zstyle :bracketed-paste-magic paste-finish pastefinish
# end of potential fix for pasting

# disable automatic cd in zsh
unsetopt AUTO_CD

# Aliases
alias gdt='git difftool'

alias mux='tmuxinator'

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
    tmux detach -E "tmux new $name"
}

function tatt() {
    if [ -z "$1" ]
    then
        name=""
    else
        name=" -t $1"
    fi
    tmux detach -E "tmux attach $name"
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
alias serv='sudo systemctl'

# Add local bin to PATH
PATH="$HOME/bin:$PATH"

# Add elixir_ls to PATH
PATH="/Users/admin/projects/personal/elixir/elixir-ls/release/:$PATH"

eval "$(direnv hook zsh)"
autoload -U +X bashcompinit && bashcompinit
eval "$($HOME/.sbsub/bin/sb init -)"

. /usr/local/opt/asdf/libexec/asdf.sh

. /usr/local/opt/asdf/etc/bash_completion.d/asdf.bash

