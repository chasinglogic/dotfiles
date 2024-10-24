# shellcheck shell=bash

ALIAS_DIR="$HOME/.alias-scripts"
export PATH="$ALIAS_DIR:$PATH"

if [[ ! -d "$ALIAS_DIR" ]]; then
	mkdir -p "$ALIAS_DIR"
else
	rm "$ALIAS_DIR"/*
fi

function add_alias() {
	script_name="$ALIAS_DIR/$1"
	cat >"$script_name" <<-EOF
		#!/usr/bin/env bash
		$2 "\$@"
	EOF
	chmod +x "$script_name"
}

add_alias ll "ls -alF"
add_alias la "ls -a"
add_alias l "ls -CF"

add_alias g "git"

add_alias venv "python3 -m venv"

if [[ "$VIM_PROG" == "nvim" ]]; then
	add_alias vimdiff "nvim -d"
fi

add_alias k 'kubectl'
add_alias logs 'kubectl logs'

add_alias tf 'terraform'

add_alias pm "podman"
add_alias dk "docker"
add_alias dce "docker compose exec -it"

add_alias mypy "dmypy check"
add_alias zyp 'sudo zypper'

add_alias pu "pulumi"
add_alias pup "pulumi up"
add_alias sync_branches "git fetch origin --prune && git branch --merged | grep -v master | xargs git branch -D"
add_alias zyp "sudo zypper"
add_alias pac "sudo pacman"

add_alias aws-login-pip "aws --profile internal-services codeartifact login --tool pip --repository tillo-python --domain tillo --domain-owner 307488140247 --region eu-west-1"
add_alias aws-login-twine "aws --profile internal-services codeartifact login --tool twine --repository tillo-python --domain tillo --domain-owner 307488140247 --region eu-west-1"
add_alias aws-login-helm "aws --profile internal-services ecr get-login-password | helm registry login --username AWS --password-stdin 307488140247.dkr.ecr.eu-west-1.amazonaws.com"
add_alias aws-login-docker "AWS_PROFILE=internal-services aws ecr get-login-password --region eu-west-1 | docker login --username AWS --password-stdin 307488140247.dkr.ecr.eu-west-1.amazonaws.com"

add_alias vim "\${VIM_PROG:-$(which vim)}"

if [[ -x "$(which zenith 2>/dev/null)" ]]; then
	add_alias htop zenith
fi

add_alias e "\$EDITOR"

# Real aliases that can't be scripts.

alias cd..="cd .."

alias apt="sudo apt"
alias dnf="sudo dnf"
alias zypper="sudo zypper"
alias pacman="sudo pacman"
if [[ "$(uname)" == "Darwin" ]]; then
	alias sed="gsed"
	alias grep="ggrep"
fi
