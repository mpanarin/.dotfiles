# Path to oh-my-zsh installation.
export ZSH=~/.oh-my-zsh


# Name of the theme to load.
ZSH_THEME="spaceship"

# Display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

plugins=(git python docker extract lol mix pip elixir asdf)

# if not Emacs - use vi-mode and start tmux
if [[ -z $INSIDE_EMACS ]]; then
    # TMUX startup
    export PATH="$PATH:/home/$USER/.gem/ruby/2.7.0/bin"
    ZSH_TMUX_AUTOSTART=false
    eval $(python3 ~/tmux_get_startup_command)
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

# Preferred editor for local and remote sessions
if [[ -z $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='vi'
fi

# TODO: Check up on this
# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

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
bindkey '\e ' autosuggest-accept

# TODO: probably not needed?
# VIRTUALENV WRAPPER STUFFS
# export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python
# source /usr/bin/virtualenvwrapper.sh

# ripgrep configurations
RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"

# asdf-vm configs
# . $HOME/.asdf/asdf.sh
# . $HOME/.asdf/completions/asdf.bash
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

# jenv for closure
export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"

# Aliases
alias cdr='cd $(git rev-parse --show-toplevel)'
alias cdrs='cd $(git rev-parse --show-toplevel)/source'

alias gdt='git difftool'

alias mux='tmuxinator'

alias doco=docker-compose

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

# needed for Zsh-vi-mode to work with autosuggest
function zvm_after_init() {
    zvm_bindkey viins '\e ' autosuggest-accept
}

# Enable fzf
export FZF_TMUX=1
zvm_after_init_commands+=('[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh')

alias tkill='tmux kill-session -t'

alias xa='exa -lh --git'
alias xat='exa -lTh --git'

alias b='bat'
alias cat='bat'

alias top='gotop'

alias ezsh='vim ~/.zshrc && source ~/.zshrc'

# Add gnubin to use make installed from homebrew
PATH="/usr/local/opt/make/libexec/gnubin:$PATH"

# Add elixir_ls to PATH
PATH="/Users/admin/projects/personal/elixir/elixir-ls/release/:$PATH"

eval "$(direnv hook zsh)"
autoload -U +X bashcompinit && bashcompinit
eval "$($HOME/.sbsub/bin/sb init -)"
