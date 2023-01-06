# Path to oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Name of the theme to load.
ZSH_THEME="spaceship"

# Display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

plugins=(git docker docker-compose extract mix mix-fast pip elixir asdf direnv)

# if not Emacs - use vi-mode and start tmux
if [[ -z $INSIDE_EMACS ]]; then
    # TMUX startup
    ZSH_TMUX_AUTOSTART=false
    eval $(~/tmux_get_startup_command)
    powerline-config tmux setup
    plugins+=(zsh-autosuggestions zsh-vi-mode zsh-syntax-highlighting)
else
    plugins+=(zsh-syntax-highlighting)
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

# remove warning on insecure completions
ZSH_DISABLE_COMPFIX=true

source $ZSH/oh-my-zsh.sh

# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -z $SSH_CONNECTION ]]; then
  export EDITOR='nvim'
else
  export EDITOR='vi'
fi

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
ZSH_AUTOSUGGEST_USE_ASYNC=1
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=30
ZSH_AUTOSUGGEST_STRATEGY=match_prev_cmd
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=0'
bindkey '^ ' autosuggest-accept

# VIRTUALENV WRAPPER STUFFS
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python
source /usr/bin/virtualenvwrapper.sh

# Enable fzf
export FZF_TMUX=1
zvm_after_init_commands+=('[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh')
# needed for Zsh-vi-mode to work with autosuggest
function zvm_after_init() {
    zvm_bindkey viins '^ ' autosuggest-accept
}

# Poetry
export PATH="$PATH:/home/$USER/.poetry/bin"

# Python startup
export PYTHONSTARTUP="$(python -m jedi repl)"

# asdf configs
. /opt/asdf-vm/asdf.sh

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

# Add elixir_ls to PATH
PATH="$HOME/projects/personal/elixir/elixir-ls/release:$PATH"

# add tmuxinator to PATH
PATH="$HOME/.local/share/gem/ruby/3.0.0/bin:$PATH"

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
alias v='vim'

alias gsubsi='g submodule init && g submodule sync && g submodule update'
alias gsubi='g submodule update --init'

alias xa='exa -lh --git'
alias xat='exa -lTh --git'

alias b='bat'
alias cat='bat'

alias ezsh='nvim ~/.zshrc && source ~/.zshrc && omz reload'
alias tkill='tmux kill-session -t'

alias kub='kubectl'
alias serv='sudo systemctl'
alias cdr='cd $(git rev-parse --show-toplevel)'

# Add local bin to PATH
PATH="$HOME/bin:$PATH"

eval "$(direnv hook zsh)"
autoload -U +X bashcompinit && bashcompinit
