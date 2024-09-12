alias ll = ls -al
alias la = ls -a
alias g = git

alias venv = python3 -m venv

alias k = kubectl
alias logs = kubectl logs

alias tf = terraform

alias pm = podman
alias dk = docker
alias dce = docker compose exec -it

alias mypy = dmypy check

alias pu = pulumi
alias pup = pulumi up

alias zyp = sudo zypper
alias zypper = sudo zypper
alias pac = sudo pacman
alias pacman = sudo pacman
alias apt = sudo apt
alias dnf = sudo dnf

alias aws-login-pip = aws --profile internal-services codeartifact login --tool pip --repository tillo-python --domain tillo --domain-owner 307488140247 --region eu-west-1
alias aws-login-twine = aws --profile internal-services codeartifact login --tool twine --repository tillo-python --domain tillo --domain-owner 307488140247 --region eu-west-1
alias aws-login-helm = aws --profile internal-services ecr get-login-password | helm registry login --username AWS --password-stdin 307488140247.dkr.ecr.eu-west-1.amazonaws.com
alias aws-login-docker = AWS_PROFILE=internal-services aws ecr get-login-password --region eu-west-1 | docker login --username AWS --password-stdin 307488140247.dkr.ecr.eu-west-1.amazonaws.com

if ((uname | get operating-system) == "Darwin") {
    alias sed = gsed
    alias grep = ggrep
}
