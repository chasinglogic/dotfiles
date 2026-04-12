# shellcheck shell=bash
#
#############
# FUNCTIONS #
#############

# Make it so we start recording our terminal as an asciicast
function start_incident {
	wezterm record
}

function dotfiles {
	cd "$(dfm where)" || return
}

function sp {
	PROJECT=""
	if [[ $1 == "" ]]; then
		PROJECT=$(projector list | fzf)
	elif [[ $1 == "home" ]]; then
		PROJECT=$HOME
	else
		PROJECT=$(projector find "(?i)$1")
	fi

	EXITCODE=$?
	if [[ $EXITCODE != 0 ]]; then
		echo "$PROJECT"
		return $EXITCODE
	fi

	cd "$PROJECT" || return
	if [ -f .env.local ]; then
		echo "Found .env.local"
		# shellcheck source=/dev/null
		source .env.local
	fi
}

function v {
	if env | grep -q '^VIRTUAL_ENV='; then
		deactivate
	fi

	TOP_LEVEL=$(git rev-parse --show-toplevel 2>/dev/null)
	if [[ -z "$TOP_LEVEL" ]]; then
		TOP_LEVEL=$(pwd)
	fi

	if [[ -f "$TOP_LEVEL/poetry.lock" ]] && command -v poetry >/dev/null 2>&1; then
		eval "$(poetry env activate)"
		return 0
	fi

	NAME=$(basename "$TOP_LEVEL")
	ENVDIR=""
	for envdir in "$TOP_LEVEL/env" "$TOP_LEVEL/venv" "$TOP_LEVEL/.venv"; do
		if [[ -d $envdir ]]; then
			ENVDIR="$envdir"
			break
		else
			ENVDIR="$envdir"
		fi
	done

	if [[ ! -d $ENVDIR ]]; then
		python3 -m venv --prompt "$NAME" "$ENVDIR"
		# shellcheck source=/dev/null
		source "$ENVDIR/bin/activate"
		pip install wheel
		return 0
	fi

	# shellcheck source=/dev/null
	source "$ENVDIR/bin/activate"
}

function kctx {
	if [[ -n "$1" ]]; then
		kubectl config use-context "$@"
	else
		kubectl config get-contexts
	fi
}

function bookmark {
	name="$1"
	if [[ -z "$name" ]]; then
		name=$(basename "$(pwd)")
	fi

	dir=$(pwd)
	echo "alias c.$name='cd $dir'" >>~/.aliases.local.sh
	# shellcheck source=/dev/null
	source ~/.aliases.local.sh
}

function color {
	for c; do
		printf '\e[48;5;%dm%03d' "$c" "$c"
	done
	printf '\e[0m \n'
}

function color_table {
	IFS=$' \t\n'
	color {0..15}
	for ((i = 0; i < 6; i++)); do
		color $(seq $((i * 36 + 16)) $((i * 36 + 51)))
	done
	color {232..255}
}

function root {
	root_dir=$(git rev-parse --show-toplevel 2>/dev/null)
	if [[ -n "$root_dir" ]]; then
		cd "$root_dir" || return
	else
		echo "Not in a git repository."
		return 1
	fi
}

function t {
	session_name="${1:-$(basename "$(pwd)")}"

	echo "Starting session: $session_name"

	if ! tmux has-session -t "$session_name" 2>/dev/null; then
		tmux new-session -d -s "$session_name"
	fi

	if [[ -n "$TMUX" ]]; then
		tmux switch-client -t "$session_name"
	else
		tmux attach-session -t "$session_name"
	fi
}

function st {
	sp "$@"
	t
}

function quiet {
	"$@" 1>/dev/null 2>/dev/null
}

function reload-bash-config {
	source "$HOME/.profile"
	source "$HOME/.functions.sh"
	# shellcheck source=/dev/null
	source "$HOME/.aliases.sh"
	source_if_exists "$HOME/.aliases.local.sh"
	source_if_exists "$HOME/.prompt.bash"
}

function fenv {
	if [[ $# -eq 0 ]]; then
		echo "usage: fenv <bash command>" >&2
		return 23
	fi

	while IFS= read -r -d '' env_var; do
		key=${env_var%%=*}
		value=${env_var#*=}

		case "$key" in
		_|PWD|SHLVL|BASHOPTS|BASHPID|BASH_ARGC|BASH_ARGV|BASH_ARGV0|BASH_CMDS|BASH_COMMAND|BASH_LINENO|BASH_SOURCE|BASH_SUBSHELL|BASH_VERSINFO|EUID|PPID|SHELLOPTS|UID)
			continue
			;;
		esac

		declare -gx "$key=$value"
	done < <(bash -lc "$* && env -0")

	return "${PIPESTATUS[0]}"
}
