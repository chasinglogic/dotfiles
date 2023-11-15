alias ll="ls -alF"
alias la="ls -a"
alias l="ls -CF"

alias cd..="cd .."

alias g="git"

alias venv="python3 -m venv"

alias vim="$VIM_PROG"
alias e="$EDITOR"
if [[ "$VIM_PROG" == "nvim" ]]; then
    alias vimdiff="nvim -d"
fi

alias k='kubectl'
alias logs='kubectl logs'

alias zyp='sudo zypper'

alias tf='terraform'


if [[ "$TERM" == "xterm-24bit" ]]; then
    alias ssh='TERM=xterm-256color ssh'
fi


alias pm="podman"
alias dk="docker"

alias circleci="NO_COLOR=1 circleci"

alias mypy="dmypy check"

alias pup="pulumi up --suppress-outputs"
alias aws-login-pip="aws --profile default codeartifact login --tool pip --repository tillo-python --domain tillo --domain-owner 411597267491 --region eu-west-1"
alias sync_branches="git fetch origin --prune && git branch --merged | grep -v master | xargs git branch -D"
