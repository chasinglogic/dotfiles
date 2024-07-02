# shellcheck: shell=bash
#
#############
# FUNCTIONS #
#############

function aws_account() {
	data=""
	if [[ -n "$1" ]]; then
		data=$(aws sts get-caller-identity --profile "$1")
	else
		data=$(aws sts get-caller-identity)
	fi

	echo "$data" | jq -r '.Account'
}

# Make it so we start recording our terminal as an asciicast
function start_incident() {
	wezterm record
}

function pyenv_activate() {
	eval "$(pyenv init -)"
}

function dotfiles() {
	cd $DOTFILES
}

function page() {
	$@ | less
}

function redact() {
	$@ 1>/dev/null 2>/dev/null
}

function st() {
	if [[ -n $1 ]]; then
		sess=$(tmux list-session -F "#S" | grep -i $1)
		tmux attach-session -t $1
	else
		if [[ -n "$TMUX" ]]; then
			tmux choose-tree -s
		else
			sess=$(tmux list-session -F "#S" | head -n1)
			tmux attach-session -t $sess \; choose-tree -s
		fi
	fi

}

function krestart() {
	DEPLOYMENT=$(kubectl get deployments $@ | fzf | awk '{ print $1 }')
	kubectl rollout restart $@ deployment/$DEPLOYMENT
}

function sp() {
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

function v() {
	if [[ -n $(env | grep 'VIRTUAL_ENV=') ]]; then
		deactivate
	fi

	TOP_LEVEL=$(git rev-parse --show-toplevel 2>/dev/null)
	if [[ $? != 0 ]]; then
		TOP_LEVEL=$(pwd)
	fi

	NAME=$(basename $TOP_LEVEL)
	ENVDIR=""
	for envdir in "$TOP_LEVEL/env" "$TOP_LEVEL/venv"; do
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

function nv() {
	if test $# -gt 0; then
		env $VIM_PROG "$@"
	elif test -f Session.vim; then
		env $VIM_PROG -S
	else
		env $VIM_PROG -c Obsession .
	fi
}

function rename_session() {
	GENERATED_SESSION_NAME=${PWD##*/}
	SESS_NAME=${1:-$GENERATED_SESSION_NAME}
	wezterm cli rename-workspace "$SESS_NAME"
}

function t() {
	rename_session
	return 0

	GENERATED_SESSION_NAME=${PWD##*/}
	SESS_NAME=${1:-$GENERATED_SESSION_NAME}
	HAS_SESSION=$(wezterm cli list | grep "$SESS_NAME")

	# TODO: find a way to attach session via CLI
	if [[ -n "$HAS_SESSION" ]]; then
		echo "Session exists!"
	else
		echo "Session is new."
	fi
	# wezterm cli spawn "$SESS_NAME"
}

function awsprof() {
	if [ $1 == "-h" ] || [ $1 == "--help" ]; then
		echo "AWS Profile Switcher:"
		echo "This tool is used for quickly switching between AWS profiles."
		echo "Usage:"
		echo "    awsprof [profilename]"
		exit 0
	fi

	export AWS_DEFAULT_PROFILE="$1"
	export AWS_EB_PROFILE="$1"
	export AWS_PROFILE="$1"
}

PULUMI_BIN=$(which pulumi 2>/dev/null)
function pulumi() {
	if [[ $@ =~ "stack select" ]]; then
		STACK_CACHE=""
	fi

	"$PULUMI_BIN" $@
}

function kctx() {
	if [[ -n "$1" ]]; then
		kubectl config use-context $@
	else
		kubectl config get-contexts
	fi
}

function bookmark() {
	name="$1"
	if [[ -z "$name" ]]; then
		name=$(basename $(pwd))
	fi

	dir=$(pwd)
	echo "alias c.$name='cd $dir'" >>~/.aliases.local.sh
	source ~/.aliases.local.sh
}
