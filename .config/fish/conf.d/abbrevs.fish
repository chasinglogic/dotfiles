abbr --add ll "ls -alF"
abbr --add la "ls -a"
abbr --add l "ls -CF"

abbr --add g git

abbr --position anywhere --add k kubectl
abbr --position anywhere --add kctx "kubectl config use-context"

abbr --add tf tofu
abbr --add tg terragrunt
abbr --add dotfiles "cd (dfm where)"

abbr --add pu pulumi
abbr --add pup "pulumi up"
abbr --add sync-branches "git fetch origin --prune && git branch --merged | grep -v master | xargs git branch -D"

abbr --add cd.. "cd .."

abbr --add apt "sudo apt"
abbr --add dnf "sudo dnf"
abbr --add zypper "sudo zypper"
abbr --add pacman "sudo pacman"
if test (uname) = Darwin
    abbr --position anywhere --add sed gsed
    abbr --position anywhere --add grep ggrep
end

function last_history_item
    echo $history[1]
end
abbr --position anywhere --add !! --function last_history_item

abbr --position anywhere --add gsutil "gcloud storage"

if command -q bat
	abbr --position anywhere --add cat bat
end
