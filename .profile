# shellcheck shell=bash

function debug() {
	LEVEL="${2:-1}"
	DEBUG_LEVEL="${CL_DEBUG:-0}"
	if [[ $LEVEL -le $DEBUG_LEVEL ]]; then
		echo "$@" 1>&2
	fi
}

function find_executable() {
	X=$(which "$1" 2>/dev/null)
	debug "Searching for $1 found: $X" 2
	echo "$X"
}

function source_if_exists() {
	if [[ -f $1 ]]; then
		debug "Sourcing $1 because it exists."
		source "$1"
	else
		debug "Not source $1 because it could not be found." 2
	fi
}

# Idempotently add directories to the path if they exist.
function add_to_path() {
	debug "Adding $1 to PATH."
	if [[ "$PATH" == *"$1"* && "$2" == "" ]]; then
		debug "$1 is already in PATH doing nothing." 2
		return 0
	elif [[ "$PATH" == *"$1"* ]]; then
		debug "$1 is already in PATH but force flag was provided to adding again." 2
	fi

	export PATH="$1:$PATH"
}

if test -n "$ZSH_VERSION"; then
	export PROFILE_SHELL=zsh
elif test -n "$BASH_VERSION"; then
	export PROFILE_SHELL=bash
else
	export PROFILE_SHELL=sh
fi

# Just show me the output please....
export AWS_PAGER=""
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
# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
export HISTCONTROL=ignoreboth
# number of commands to save in history file
export HISTSIZE=1000000
# number of lines to save in history file
export HISTFILESIZE=$HISTSIZE
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
# Make ripgrep use my config file.
export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"
# Make XDG_CONFIG_HOME the same on all platforms.
export XDG_CONFIG_HOME="$HOME/.config"

# Mac specific fixes
if [[ "$(uname)" == "Darwin" ]]; then
	export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES
	export CLICOLOR=1
	export GRPC_PYTHON_BUILD_SYSTEM_OPENSSL=1
	export GRPC_PYTHON_BUILD_SYSTEM_ZLIB=1
	export LDFLAGS="-L/opt/homebrew/opt/openssl@3/lib"
	export CPPFLAGS="-I/opt/homebrew/opt/openssl@3/include"

	if [[ -d "$HOME/Library/Python" ]]; then
		for dir in $(find "$HOME/Library/Python" -maxdepth 1 -type d); do
			export PATH="$PATH:$dir/bin"
		done
	fi

	if [[ -d "/usr/local/opt/postgresql@16" ]]; then
		export LDFLAGS="$LDFLAGS -L/usr/local/opt/postgresql@16/lib"
		export CPPFLAGS="$CPPFLAGS -I/usr/local/opt/postgresql@16/include"
		export PKG_CONFIG_PATH="/usr/local/opt/postgresql@16/lib/pkgconfig"
		add_to_path "/usr/local/opt/postgresql@16/bin"
	fi

	if [[ -d "/opt/homebrew/opt/postgresql@16" ]]; then
		export LDFLAGS="$LDFLAGS -L/opt/homebrew/opt/postgresql@16/lib"
		export CPPFLAGS="$CPPFLAGS -I/opt/homebrew/opt/postgresql@16/include"
		export PKG_CONFIG_PATH="/opt/homebrew/opt/postgresql@16/lib/pkgconfig"
		add_to_path "/opt/homebrew/opt/postgresql@16/bin"
	fi

fi

export COLORTERM=truecolor

# Storage for miscellaneous or system specific environment variables
source_if_exists "$HOME/.env.bash"
# Setup rustup, cargo path
source_if_exists /home/chasinglogic/.rustrc

add_to_path /opt/homebrew/bin
add_to_path "$HOME/.local/bin"
add_to_path "$HOME/.elixir-ls/dist"
add_to_path "$GOPATH/bin"
add_to_path "$HOME/.cargo/bin"
add_to_path "/Applications/PyCharm CE.app/Contents/MacOS"
add_to_path "/Applications/PyCharm.app/Contents/MacOS"
add_to_path "/Applications/Docker.app/Contents/Resources/bin"
add_to_path "$HOME/.pulumi/bin"

source_if_exists "$HOME/.cargo/env"
source_if_exists "$HOME/.ghcup/env"

# Enable nix if I've installed it on this system
# Comes after the add_to_path so that nix beats these in the $PATH race.
source_if_exists "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
source_if_exists "$HOME/.nix-profile/etc/profile.d/nix.sh"
source_if_exists /etc/profile.d/nix.sh
source_if_exists /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh

# Environment variables that are local to this machine and not synced with dfm.
source_if_exists "$HOME/.env.local"

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
[ -x /usr/bin/dircolors ] && eval "alias ls='ls --color'"
source_if_exists "$HOME/.aliases.sh"
source_if_exists "$HOME/.aliases.local.sh"

if [ -d "$HOME/.krew" ]; then
	export PATH="${HOME}/.krew/bin:$PATH"
fi

# This has to be after the $PATH is set up.
# FZF default find command
if [[ -n $(find_executable fd) ]]; then
	export FZF_DEFAULT_COMMAND="fd --type f --hidden --exclude '.git/'"
else
	export FZF_DEFAULT_COMMAND="find . -path './.git' -prune -o -type f -print"
fi

if [[ -n $(find_executable nvim) ]]; then
	export VIM_PROG="nvim"
else
	export VIM_PROG="vim"
fi

if [[ "$EDITOR" != "code --wait" ]]; then
	export EDITOR="$VIM_PROG"
fi

debug "PATH=$PATH" 2
. "$HOME/.cargo/env"
