# shellcheck: shell=bash
#
#############
# FUNCTIONS #
#############

# Make it so we start recording our terminal as an asciicast
function start_incident {
	wezterm record
}

function dotfiles {
	cd $(dfm where)
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

	if [[ $? != 0 ]]; then
		EXITCODE=$?
		echo $PROJECT
		return $EXITCODE
	fi

	cd $PROJECT
	if [ -f .env.local ]; then
		echo "Found .env.local"
		source .env.local
	fi
}

function v {
	if [[ -n $(env | grep 'VIRTUAL_ENV=') ]]; then
		deactivate
	fi

	TOP_LEVEL=$(git rev-parse --show-toplevel 2>/dev/null)
	if [[ $? != 0 ]]; then
		TOP_LEVEL=$(pwd)
	fi

	NAME=$(basename $TOP_LEVEL)
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
		python3 -m venv --prompt $NAME $ENVDIR
		source $ENVDIR/bin/activate
		pip install wheel
		return 0
	fi

	source $ENVDIR/bin/activate
}

function kctx {
	if [[ -n "$1" ]]; then
		kubectl config use-context $@
	else
		kubectl config get-contexts
	fi
}

function bookmark {
	name="$1"
	if [[ -z "$name" ]]; then
		name=$(basename $(pwd))
	fi

	dir=$(pwd)
	echo "alias c.$name='cd $dir'" >>~/.aliases.local.sh
	source ~/.aliases.local.sh
}

function color {
	for c; do
		printf '\e[48;5;%dm%03d' $c $c
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
