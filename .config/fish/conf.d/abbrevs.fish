abbr --add ll "ls -alF"
abbr --add la "ls -a"
abbr --add l "ls -CF"

abbr --add g git

abbr --add venv "python3 -m venv"

abbr --add k kubectl
abbr --add kctx "kubectl config use-context"

abbr --add logs 'kubectl logs'

abbr --add tf tofu
abbr --add tg terragrunt
abbr --add grunt terragrunt
abbr --add dotfiles "cd (dfm where)"

abbr --add pm podman
abbr --add dk docker
abbr --add dce "docker compose exec -it"

abbr --add mypy "dmypy check"
abbr --add zyp 'sudo zypper'

abbr --add pu pulumi
abbr --add pup "pulumi up"
abbr --add sync-branches "git fetch origin --prune && git branch --merged | grep -v master | xargs git branch -D"
abbr --add zyp "sudo zypper"
abbr --add pac "sudo pacman"

abbr --add aws-login-pip "aws --profile internal-services codeartifact login --tool pip --repository tillo-python --domain tillo --domain-owner 307488140247 --region eu-west-1"
abbr --add aws-login-twine "aws --profile internal-services codeartifact login --tool twine --repository tillo-python --domain tillo --domain-owner 307488140247 --region eu-west-1"
abbr --add aws-login-helm "aws --profile internal-services ecr get-login-password | helm registry login --username AWS --password-stdin 307488140247.dkr.ecr.eu-west-1.amazonaws.com"
abbr --add aws-login-docker "AWS_PROFILE=internal-services aws ecr get-login-password --region eu-west-1 | docker login --username AWS --password-stdin 307488140247.dkr.ecr.eu-west-1.amazonaws.com"

abbr --add vim "\${VIM_PROG:-\$(which vim)}"

if command -q zenith
    abbr --add htop zenith
end

abbr --add e "\$EDITOR"

abbr --add cd.. "cd .."

abbr --add apt "sudo apt"
abbr --add dnf "sudo dnf"
abbr --add zypper "sudo zypper"
abbr --add pacman "sudo pacman"
if test (uname) = Darwin
    abbr --add sed gsed
    abbr --add grep ggrep
end

if command -q hwatch
    abbr --add watch hwatch
end

abbr --add start-incident "wezterm record"
