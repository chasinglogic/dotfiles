abbr --position anywhere --add ll "ls -alF"
abbr --position anywhere --add la "ls -a"
abbr --position anywhere --add l "ls -CF"

abbr --position anywhere --add g git

abbr --position anywhere --add venv "python3 -m venv"

abbr --position anywhere --add k kubectl
abbr --position anywhere --add kctx "kubectl config use-context"

abbr --position anywhere --add tf tofu
abbr --position anywhere --add tg terragrunt
abbr --position anywhere --add grunt terragrunt
abbr --position anywhere --add dotfiles "cd (dfm where)"

abbr --position anywhere --add pm podman
abbr --position anywhere --add dk docker
abbr --position anywhere --add dce "docker compose exec -it"

abbr --position anywhere --add mypy "dmypy check"
abbr --position anywhere --add zyp 'sudo zypper'

abbr --position anywhere --add pu pulumi
abbr --position anywhere --add pup "pulumi up"
abbr --position anywhere --add sync-branches "git fetch origin --prune && git branch --merged | grep -v master | xargs git branch -D"
abbr --position anywhere --add zyp "sudo zypper"
abbr --position anywhere --add pac "sudo pacman"

abbr --position anywhere --add aws-login-pip "aws --profile internal-services codeartifact login --tool pip --repository tillo-python --domain tillo --domain-owner 307488140247 --region eu-west-1"
abbr --position anywhere --add aws-login-twine "aws --profile internal-services codeartifact login --tool twine --repository tillo-python --domain tillo --domain-owner 307488140247 --region eu-west-1"
abbr --position anywhere --add aws-login-helm "aws --profile internal-services ecr get-login-password | helm registry login --username AWS --password-stdin 307488140247.dkr.ecr.eu-west-1.amazonaws.com"
abbr --position anywhere --add aws-login-docker "AWS_PROFILE=internal-services aws ecr get-login-password --region eu-west-1 | docker login --username AWS --password-stdin 307488140247.dkr.ecr.eu-west-1.amazonaws.com"

abbr --position anywhere --add vim $VIM_PROG

if command -q zenith
    abbr --position anywhere --add htop zenith
end

abbr --position anywhere --add e $EDITOR

abbr --position anywhere --add cd.. "cd .."

abbr --position anywhere --add apt "sudo apt"
abbr --position anywhere --add dnf "sudo dnf"
abbr --position anywhere --add zypper "sudo zypper"
abbr --position anywhere --add pacman "sudo pacman"
if test (uname) = Darwin
    abbr --position anywhere --add sed gsed
    abbr --position anywhere --add grep ggrep
end

if command -q hwatch
    abbr --position anywhere --add watch hwatch
end

abbr --position anywhere --add start-incident "wezterm record"

function last_history_item
    echo $history[1]
end
abbr --position anywhere --add !! --function last_history_item
