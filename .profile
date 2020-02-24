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

### Set TERM

# Linux
if [[ "$COLORTERM" == "truecolor" ]] && [ -f "$HOME/.terminfo/x/xterm-24bit" ]; then
    export TERM="xterm-24bit"
elif [ -n "$VTE_VERSION" ] && ([ -z "$TERM" ] || [ "$TERM" != "tmux-256color" ]); then
    export TERM="vte-256color"
fi

# For Mac I set TERM to iterm2 via it's preferences.

# Fallback
if [ -z "$TERM" ]; then
    export TERM="xterm-256color"
fi

if [[ -x $(which emacsclient) ]]; then
    export EDITOR="emacs -nw"
elif [[ -x $(which nvim) ]]; then
    export EDITOR="nvim"
else
    export EDITOR="vi"
fi

# Mac specific fixes
if [[ "$(uname)" == "Darwin" ]]; then
    export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES
    export CLICOLOR=1
fi

# Storage for miscellaneous or system specific environment variables
source_if_exists $HOME/.env.bash
# Enable nix if I've installed it on this system
source_if_exists $HOME/.nix-profile/etc/profile.d/nix.sh

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

# FZF default find command
export FZF_DEFAULT_COMMAND="find . -path './.git' -prune -o -type f -print"

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
[ -x /usr/bin/dircolors ] && eval "alias ls='ls --color'"

export PATH="$HOME/.cargo/bin:$PATH"
export PLASMA_USE_QT_SCALING=1

export VIRTUALENVWRAPPER_PYTHON="$(which python3)"
