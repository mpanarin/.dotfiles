# Path to oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Name of the theme to load.
ZSH_THEME="spaceship"

# Display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

plugins=(git docker docker-compose extract mix mix-fast pip asdf direnv)

# if not Emacs - use vi-mode and start tmux
if [[ -z $INSIDE_EMACS ]]; then
    # TMUX startup
    ZSH_TMUX_AUTOSTART=false
    eval $(~/tmux_get_startup_command)
    powerline-config tmux setup
    plugins+=(
        zsh-autosuggestions
        zsh-syntax-highlighting
    )
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

# Enable zsh autosuggestions
ZSH_AUTOSUGGEST_USE_ASYNC=1
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=30
ZSH_AUTOSUGGEST_STRATEGY=match_prev_cmd
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=0'

# VIRTUALENV WRAPPER STUFFS
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python
source /usr/bin/virtualenvwrapper.sh

# Enable fzf
export FZF_TMUX=1
export FZF_TMUX_OPTS='-p 100%,40% -y P'
source ~/.fzf.zsh
bindkey '^ ' autosuggest-accept

# Poetry
export PATH="$PATH:/home/$USER/.poetry/bin"

# Python startup
export PYTHONSTARTUP="$(python -m jedi repl)"

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

# GOOGLE CLOUD STUFF
CLOUDSDK_ROOT_DIR=/opt/google-cloud-cli
CLOUDSDK_PYTHON=/usr/bin/python
CLOUDSDK_PYTHON_ARGS='-S -W ignore'
PATH="$CLOUDSDK_ROOT_DIR/bin:$PATH"
GOOGLE_CLOUD_SDK_HOME=$CLOUDSDK_ROOT_DIR

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

source ~/.dotfiles/aws_profile.zsh

alias devops-profile=''

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

alias kube35aws='kubectl --kubeconfig ~/projects/35up/tatenen/staging/kubeconfig'
alias kube35aws_prod='kubectl --kubeconfig ~/projects/35up/tatenen/production/kubeconfig'
alias kube35gke='kubectl --kubeconfig ~/projects/35up/tatenen/staging/kubeconfig-gke'
alias kub='kubectl'
alias serv='sudo systemctl'
alias cdr='cd $(git rev-parse --show-toplevel)'

# Add local bin to PATH
PATH="$HOME/bin:$PATH"

eval "$(direnv hook zsh)"
autoload -U +X bashcompinit && bashcompinit

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/mpanarin/google-cloud-sdk/path.zsh.inc' ]; then . '/home/mpanarin/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/mpanarin/google-cloud-sdk/completion.zsh.inc' ]; then . '/home/mpanarin/google-cloud-sdk/completion.zsh.inc'; fi
