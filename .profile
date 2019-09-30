function source_if_exists() {
    if [[ -f $1 ]]; then
        source $1
    fi
}

function add_to_path() {
    if [[ "$PATH" != "${PATH/$1/}" ]]; then
        return 0;
    fi

    if [[ -d $1 ]]; then
        export PATH="$1:$PATH"
    fi
}

export PACKER_NO_COLOR="1"

export GPG_TTY=$(tty)

export CARGOBIN="$HOME/.cargo/bin"

export GOPATH="$HOME/Code/go"

export LANG=en_US.UTF-8
export LC_ALL="en_US.UTF-8"

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
export HISTCONTROL=ignoreboth
# number of commands to save in history file
export HISTSIZE=1000
# number of lines to save in history file
export HISTFILESIZE=2000

### Set TERM

# Linux
if [ -n "$VTE_VERSION" ] && ([ -z "$TERM" ] || [ "$TERM" != "tmux-256color" ]); then
    export TERM="vte-256color"
fi

# For Mac I set TERM to iterm2 via it's preferences.

# Fallback
if [ -z "$TERM" ]; then
    export TERM="xterm-256color"
fi

# FZF default find command
export FZF_DEFAULT_COMMAND="find . -path './.git' -prune -o -type f -print"
export EDITOR="nvim"

# Mac specific fixes
if [[ "$(uname)" == "Darwin" ]]; then

export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES

export CLICOLOR=1

fi

source_if_exists $HOME/.env.bash

source_if_exists $HOME/.nix-profile/etc/profile.d/nix.sh

add_to_path $CARGOBIN
add_to_path $GOPATH/bin
add_to_path $HOME/.cargo/bin
add_to_path $HOME/.local/bin
add_to_path /Users/chasinglogic/.cask/bin
add_to_path /home/chasinglogic/.cask/bin
add_to_path /opt/local/bin
add_to_path /usr/local/bin
add_to_path /usr/local/sbin
add_to_path /usr/bin
add_to_path /bin
add_to_path /usr/lib/icecream/bin
add_to_path $HOME/Library/Python/3.7/bin

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
[ -x /usr/bin/dircolors ] && eval "alias ls='ls --color'"

export PATH="$HOME/.cargo/bin:$PATH"
