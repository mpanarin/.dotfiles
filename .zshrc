# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="spaceship"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git python vi-mode)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
 if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='nano'
 else
   export EDITOR='vim'
 fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
#
source ~/.gem/ruby/2.5.0/gems/tmuxinator-0.12.0/completion/tmuxinator.zsh
ZSH_TMUX_AUTOSTART=false
[[ $TMUX == "" ]] && tmux new-session
powerline-config tmux setup

SPACESHIP_USER_SHOW=false
SPACESHIP_HG_SHOW=false
SPACESHIP_HG_STATUS_SHOW=false
SPACESHIP_PACKAGE_SHOW=false
SPACESHIP_NODE_SHOW=false
SPACESHIP_RUBY_SHOW=false
SPACESHIP_ELIXIR_SHOW=false
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

source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_USE_ASYNC=1
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=30
bindkey '^ ' autosuggest-accept

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# VIRTUALENV WRAPPER STUFFS
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python
source /usr/bin/virtualenvwrapper.sh

alias gdt='git difftool'

alias doco=docker-compose
alias doco_rebuild='doco down -v && doco up --build'
alias doco_templatedb='doco run --rm odoo createdb -T odoodb'
alias doco_dropdb='doco run --rm odoo dropdb'

function doco_run() {
    if [ -z "$1" ]
    then
        db=odoodb
    else
        db=$1
    fi
    if [ -z "$2" ]
    then
        port=80
    else
        port=$1
    fi
    doco run --rm -e DB_NAME=$db -p $port:8069 odoo odoo --workers=0
}

function doco_run_dev() {
    if [ -z "$1" ]
    then
        db=odoodb
    else
        db=$1
    fi
    if [ -z "$2" ]
    then
        port=80
    else
        port=$1
    fi
    doco run --rm -e DB_NAME=$db -p $port:8069 odoo odoo --workers=0 --dev=qweb,xml
}

function doco_old_run() {
    if [ -z "$1" ]
    then
        db=odoodb
    else
        db=$1
    fi
    if [ -z "$2" ]
    then
        port=80
    else
        port=$2
    fi
    doco run --rm -e DB_NAME=$db -p $port:8069 odoo odoo --workers=0 --dev
}

function doco_migrate() {
    if [ -z "$1" ]
    then
        return "no db name provided"
    else
        db=$1
    fi
    if [ -z "$2" ]
    then
        return "no version specified"
    else
        version=$2
    fi
    doco run --rm -e MARABUNTA_FORCE_VERSION=$version -e DB_NAME=$db odoo migrate
}

alias doco_log='docker-compose logs'

function doco_sh() {
    if [ -z "$1" ]
    then
        db=odoodb
    else
        db=$1
    fi
    doco run --rm -e DB_NAME=$db odoo odoo shell
}

function doco_ant() {
    if [ -z "$1" ]
    then
        echo "no db name"
    else
        doco run --rm -e DB_NAME=$1 odoo anthem $2
    fi
}

function doco_psql() {
    if [ -z "$1" ]
    then
        echo "no container name"
    else
        docker exec -it $1 psql -U odoo
    fi
}

function doco_bash() {
    if [ -z "$1" ]
    then
        echo "no container name"
    else
        docker exec -it $1 bash
    fi
}
# setup test database. Just run `dood_test_setup`
alias dood_test_setup='docker-compose run --rm -e DB_NAME=testdb odoo testdb-gen -i base'
# reuse testdb and install or update modules on demand. Just run `dood_test_update -i/u something`
alias dood_test_update='docker-compose run --rm -e DB_NAME=testdb odoo testdb-update'
# run tests using pytest. Just run `dood_test_run path/to/your/module`
# NOTE: you need to run dood_test_update 1st IF xml or models have been updated
alias dood_test_run='docker-compose run --rm -e DB_NAME=testdb odoo pytest -s'
# run tests using std odoo test machinery (eg: you need an HttpCase). Just run `dood_test_run_odoo -u module`
alias dood_test_run_odoo='docker-compose run --rm -e DEMO=True -e DB_NAME=testdb -e MIGRATE=False odoo odoo --workers=0 --test-enable --stop-after-init'

function dood_test_with_cov() {
    if [ -z "$1" ]
    then
        echo 'no module path'
    else
        doco run --rm -e DB_NAME=testdb odoo pytest -s $1 --cov=$1
    fi
}

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

alias dood_test_run_travis='docker-compose run --rm odoo runtests'
alias gsubsi='g submodule init && g submodule sync && g submodule update'
alias gsubi='g submodule update --init'

alias xa='exa -lh --git'
alias xat='exa -lTh --git'

alias ezsh='vim ~/.zshrc'
alias tkill='tmux kill-session -t'

export FZF_TMUX=1
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
