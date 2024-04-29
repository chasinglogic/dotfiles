# shellcheck shell=bash

alias ll="ls -alF"
alias la="ls -a"
alias l="ls -CF"

alias cd..="cd .."

alias g="git"

alias venv="python3 -m venv"

if [[ -n "$VIM_PROG" ]]; then	
    alias vim="\$VIM_PROG"
fi

alias e="\$EDITOR"
if [[ "$VIM_PROG" == "nvim" ]]; then
    alias vimdiff="nvim -d"
fi

alias k='kubectl'
alias logs='kubectl logs'

alias tf='terraform'

alias pm="podman"
alias dk="docker"

alias circleci="NO_COLOR=1 circleci"

alias mypy="dmypy check"
alias zyp='sudo zypper'

alias pup="pulumi up --suppress-outputs"
alias sync_branches="git fetch origin --prune && git branch --merged | grep -v master | xargs git branch -D"
alias apt="sudo apt"
alias dnf="sudo dnf"
alias zyp="sudo zypper"
alias zypper="sudo zypper"

alias aws-login-pip="aws --profile internal-services codeartifact login --tool pip --repository tillo-python --domain tillo --domain-owner 307488140247 --region eu-west-1"
alias aws-login-twine="aws --profile internal-services codeartifact login --tool twine --repository tillo-python --domain tillo --domain-owner 307488140247 --region eu-west-1"
alias aws-login-helm="aws --profile internal-services ecr get-login-password | helm registry login --username AWS --password-stdin 307488140247.dkr.ecr.eu-west-1.amazonaws.com"
alias aws-login-docker="aws --profile internal-services ecr get-login-password --region eu-west-1 | docker login --username AWS --password-stdin 307488140247.dkr.ecr.eu-west-1.amazonaws.com"
