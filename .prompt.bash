##########
# PROMPT #
##########

COMMAND_STATUS_COLOR="\$(tput bold)\$(tput setaf 5)"
HOSTNAME_COLOR="\$(tput setaf 2)"
PWD_COLOR="\$(tput setaf 6)"
GIT_BRANCH_COLOR="\$(tput setaf 1)"
LAMBDA_COLOR="\$(tput setaf 3)"
USERNAME_COLOR="\$(tput setaf 13)"
NO_COLOR="\e[0m"

function parse_git_branch {
    ref=$(git symbolic-ref HEAD 2> /dev/null) || return
    echo "${ref#refs/heads/} "
}

function asterisk_if_dirty {
    [[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]] && echo "*"
}

function lambda_or_delta {
    if [[ $(asterisk_if_dirty) == "*" ]]; then
        echo "Δ"
        return
    fi
    echo "λ"
}

function last_command_status {
    if [[ $? == "0" ]]; then
        return
    fi

    echo "!! "
}


function kube_context {
    if [[ -x $(which kubectl 2>/dev/null) ]]; then
        current_context=$(kubectl config get-contexts | grep '*' | awk '{ print $2 }')
        echo "(kube: $current_context) "
    fi
}

PS1="\[$COMMAND_STATUS_COLOR\]\$(last_command_status)\$(kube_context)\[$USERNAME_COLOR\]\u\[$LAMBDA_COLOR\]@\[$HOSTNAME_COLOR\]\H\[$PWD_COLOR\] \w \[$GIT_BRANCH_COLOR\]\$(parse_git_branch)\[$LAMBDA_COLOR\]\$(lambda_or_delta) \[$NO_COLOR\]"
