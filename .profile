set -o emacs

function find_executable() {
    X=$(which $1 2>/dev/null)
    echo $X
}

function source_if_exists() {
    if [[ -f $1 ]]; then
        source $1
    fi
}

# Idempotently add directories to the path if they exist.
function add_to_path() {
    if [[ "$PATH" != "${PATH/$1;/}" ]]; then
        return 0;
    fi

    if [[ -d $1 ]]; then
        export PATH="$1:$PATH"
    fi
}

# Node version manager storage location
export NVM_DIR="$HOME/.nvm"
export NOTES_DIR="$HOME/Dropbox/Notes"
# Use Python3 for Virtualenvwrapper
export VIRTUALENVWRAPPER_PYTHON="$(which python3)"
# Plasma scale with HIDPI
export PLASMA_USE_QT_SCALING=1
# Packer's colorized output messes with terminals and other programs I use.
export PACKER_NO_COLOR="1"
# GPG can weirdly hang without this I've found
export GPG_TTY=$(tty)
# Needed for the go compiler and tooling
export GOPATH="$HOME/Code/go"
# Set LANG and Locale so it's always what I expect
export LANG=en_US.UTF-8
export LC_ALL="en_US.UTF-8"
# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
export HISTCONTROL=ignoreboth
# number of commands to save in history file
export HISTSIZE=1000
# number of lines to save in history file
export HISTFILESIZE=2000
# Tell my update script where to find my dotfile repository
export DOTFILES_REPOSITORY="$HOME/Code/dotfiles"
# Set CCACHE directory
if [ -d /data/ccache ]; then
    export CCACHE_DIR=/data/ccache
    export CCACHE_MAXSIZE=200G
else
    export CCACHE_MAXSIZE=20G
fi
# Make Helm look for my local tiller server
export HELM_HOST=localhost:44134
# Make Firefox use Wayland
export MOZ_ENABLE_WAYLAND=1

# Find the vim to use.
if [[ -n $(find_executable nvim) ]]; then
    export VIM_PROG=nvim
elif [[ -n $(find_executable vim) ]]; then
    export VIM_PROG=vim
else
    export VIM_PROG=vi
fi

if [[ -n $(find_executable emacsclient) ]]; then
    export EDITOR="emacsclient -a '' -nw"
else
    export EDITOR="$VIM_PROG"
fi

# Mac specific fixes
if [[ "$(uname)" == "Darwin" ]]; then
    export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES
    export CLICOLOR=1
fi

if [[ -f ~/.terminfo/x/xterm-24bit  ]]; then
    export TERM="xterm-24bit"
fi

# Storage for miscellaneous or system specific environment variables
source_if_exists $HOME/.env.bash
# Enable nix if I've installed it on this system
source_if_exists $HOME/.nix-profile/etc/profile.d/nix.sh
# Set up virtualenvwrapper
# source_if_exists /usr/local/bin/virtualenvwrapper.sh
# source_if_exists $HOME/.local/bin/virtualenvwrapper.sh
source_if_exists $NVM_DIR/nvm.sh  # This loads nvm
source_if_exists $NVM_DIR/bash_completion  # This loads nvm bash_completion
if [[ -z $NVM_BIN ]]; then
    if [[ ! -f $(nvm which 'lts/*') ]]; then
        nvm install --lts
    fi

    nvm use --lts
fi

add_to_path /snap/bin
add_to_path /opt/local/bin
add_to_path /usr/local/bin
add_to_path /usr/local/sbin
add_to_path /usr/bin
add_to_path /usr/lib/icecream/bin
add_to_path $GOPATH/bin
add_to_path $HOME/.cargo/bin
add_to_path $HOME/.local/bin
add_to_path $HOME/.cask/bin
add_to_path $HOME/.cask/bin
add_to_path $HOME/Library/Python/3.7/bin
add_to_path $HOME/.pyenv/bin

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
[ -x /usr/bin/dircolors ] && eval "alias ls='ls --color'"

export PATH="$HOME/.cargo/bin:$PATH"

# This has to be after the $PATH is set up.
# FZF default find command
if [[ -n $(find_executable fd) ]]; then
    export FZF_DEFAULT_COMMAND="fd --type f --hidden --exclude '.git/'"
else
    export FZF_DEFAULT_COMMAND="find . -path './.git' -prune -o -type f -print"
fi

if [[ -n $(find_executable dfm) ]]; then
    export DOTFILES=$(dfm where)
fi
