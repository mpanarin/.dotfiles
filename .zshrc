# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
  export ZSH=/home/m-panarin/.oh-my-zsh

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
plugins=(git python)

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
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

source /var/lib/gems/2.3.0/gems/tmuxinator-0.10.1/completion/tmuxinator.zsh
ZSH_TMUX_AUTOSTART=false
[[ $TMUX == "" ]] && tmux new-session

source "/home/m-panarin/.oh-my-zsh/custom/themes/spaceship.zsh-theme"
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

alias gdt='git difftool'

alias doco=docker-compose
alias doco_templatedb='doco run --rm odoo createdb -T odoodb'
alias doco_dropdb='doco run --rm odoo dropdb'

function doco_run() {
    if [ $1 != "" ]
    then
        db=$1
    else
        db=odoodb
    fi
    doco run --rm -e DB_NAME=$db -p 80:8069 odoo odoo --workers=0
}

function doco_migrate() {
    if [ $1 != "" ]
    then
        db=$1
    else
        return "no db name"
    fi
    if [ $2 != "" ]
    then
        version=$2
    else
        return "no version specified"
    fi
    doco run --rm -e MARABUNTA_FORCE_VERSION=$version -e DB_NAME=$db odoo migrate
}

alias doco_log='docker-compose logs'

function doco_sh() {
    if [ $1 != "" ]
    then
        db=$1
    else
        db=odoodb
    fi
    doco run --rm -e DB_NAME=$db odoo odoo shell
}

function doco_ant() {
    if [ $1 != "" ]
    then
        db=$1
    else
        return "no db name"
    fi
    doco run --rm -e DB_NAME=$db odoo anthem
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

alias dood_test_run_travis='docker-compose run --rm odoo runtests'
alias gsubi='git submodule init && git submodule sync && git submodule update'
