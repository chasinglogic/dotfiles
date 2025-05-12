# shellcheck shell=bash

alias ll="ls -alF"
alias la="ls -a"
alias l="ls -CF"

alias g="git"

alias venv="python3 -m venv"

if [[ "$VIM_PROG" == "nvim" ]]; then
	alias vimdiff="nvim -d"
fi

alias k='kubectl'
alias logs='kubectl logs'

alias tf='tofu'
alias tg='terragrunt'
alias grunt='terragrunt'

alias pm="podman"
alias dk="docker"
alias dce="docker compose exec -it"

alias mypy="dmypy check"
alias zyp='sudo zypper'

alias pu="pulumi"
alias pup="pulumi up"
alias sync_branches="git fetch origin --prune && git branch --merged | grep -v master | xargs git branch -D"
alias zyp="sudo zypper"
alias pac="sudo pacman"

alias aws-login-pip="aws --profile internal-services codeartifact login --tool pip --repository tillo-python --domain tillo --domain-owner 307488140247 --region eu-west-1"
alias aws-login-twine="aws --profile internal-services codeartifact login --tool twine --repository tillo-python --domain tillo --domain-owner 307488140247 --region eu-west-1"
alias aws-login-helm="aws --profile internal-services ecr get-login-password | helm registry login --username AWS --password-stdin 307488140247.dkr.ecr.eu-west-1.amazonaws.com"
alias aws-login-docker="AWS_PROFILE=internal-services aws ecr get-login-password --region eu-west-1 | docker login --username AWS --password-stdin 307488140247.dkr.ecr.eu-west-1.amazonaws.com"

alias vim="\${VIM_PROG:-\$(which vim)}"

if [[ -x "$(which zenith 2>/dev/null)" ]]; then
	alias htop="zenith"
fi

alias e="\$EDITOR"

alias cd..="cd .."

alias apt="sudo apt"
alias dnf="sudo dnf"
alias zypper="sudo zypper"
alias pacman="sudo pacman"
if [[ "$(uname)" == "Darwin" ]]; then
	alias sed="gsed"
	alias grep="ggrep"
fi

if [[ -x "$(find_executable hwatch)" ]]; then
	alias watch="hwatch" 
fi
