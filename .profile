function debug() {
    if [[ -n $CL_DEBUG ]]; then
        echo $@ 1>&2
    fi
}

function find_executable() {
    X=$(which $1 2>/dev/null)
    debug "Searching for $1 found: $X"
    echo $X
}

function source_if_exists() {
    if [[ -f $1 ]]; then
        debug "Sourcing $1 because it exists."
        source $1
    else
        debug "Not source $1 because it could not be found."
    fi
}

# Idempotently add directories to the path if they exist.
function add_to_path() {
    debug "Adding $1 to PATH."
    if [[ "$PATH" == *"$1"* && "$2" == "" ]]; then
        debug "$1 is already in PATH doing nothing."
        return 0;
    elif [[ "$PATH" == *"$1"* ]]; then
        debug "$1 is already in PATH but force flag was provided to adding again."
    fi

    if [[ -d "$1" ]]; then
        debug "$1 exists adding to beginning of PATH."
        export PATH="$1:$PATH"
    else
        debug "$1 does not exist."
    fi
}

set -o emacs

# Google should just make this default since it is required...
export USE_GKE_GCLOUD_AUTH_PLUGIN=True
# Node version manager storage location
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
# Make helm work with our internal chart museum
export GODEBUG=x509ignoreCN=0
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

if [[ "$EDITOR" != "code --wait" ]]; then
    export EDITOR="emacsclient -c -nw -a 'emacs -nw'"
fi

# Mac specific fixes
if [[ "$(uname)" == "Darwin" ]]; then
    export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES
    export CLICOLOR=1
    export GRPC_PYTHON_BUILD_SYSTEM_OPENSSL=1
    export GRPC_PYTHON_BUILD_SYSTEM_ZLIB=1
    export LDFLAGS="-L/opt/homebrew/opt/openssl@3/lib"
    export CPPFLAGS="-I/opt/homebrew/opt/openssl@3/include"

    for dir in $(find "$HOME/Library/Python" -maxdepth 1 -type d); do
        export PATH="$PATH:$dir/bin"
    done
else
    # Inform Emacs and other programs they can use truecolor
    export COLORTERM=truecolor
fi

# Storage for miscellaneous or system specific environment variables
source_if_exists $HOME/.env.bash
# Setup rustup, cargo path
source_if_exists /home/chasinglogic/.rustrc
source_if_exists "$HOME/.cargo/env"

add_to_path "$HOME/.elixir-ls/dist"
add_to_path "$GOPATH/bin"
add_to_path "$HOME/.cargo/bin"
add_to_path "$HOME/.local/bin"
add_to_path "$HOME/.cask/bin"
add_to_path "$HOME/.mpb/common-be-scripts"
add_to_path $HOME/.rbenv/bin
add_to_path /opt/homebrew/bin
add_to_path /opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin/

# Enable nix if I've installed it on this system
# Comes after the add_to_path so that nix beats these in the $PATH race.
source_if_exists "$HOME/.nix-profile/etc/profile.d/nix.sh"
source_if_exists /etc/profile.d/nix.sh
source_if_exists /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh

# Enable asdf
source_if_exists "$HOME/.asdf/asdf.sh"
# asdf is a little too clever for it's own good and so somehow the shims / bin
# won't always end up at the beginning of PATH so this forces that to be true.
add_to_path "$ASDF_DIR/bin" true
add_to_path "${ASDF_DATA_DIR:-$HOME/.asdf}/shims" true

source_if_exists "$HOME/.env.local"

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
[ -x /usr/bin/dircolors ] && eval "alias ls='ls --color'"

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

ssh-add $HOME/.ssh/id_rsa >/dev/null 2>/dev/null
