COMMAND_STATUS_COLOR="\$(tput setaf 5)"
HOSTNAME_COLOR="\$(tput setaf 2)"
PWD_COLOR="\$(tput setb)\$(tput setaf 6)"
GIT_BRANCH_COLOR="\$(tput setaf 1)"
LAMBDA_COLOR="\$(tput setaf 3)"
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

# Make the pwd look pretty
function pretty_pwd {
    dir=`pwd | awk -F\/ '{print $(NF-1),$(NF)}' | sed 's/ /\\//'`
    echo "$dir"
}

PS1="\[$COMMAND_STATUS_COLOR\]\$(last_command_status)\[$HOSTNAME_COLOR\]@$HOSTNAME\[$PWD_COLOR\] \$(pretty_pwd) \[$GIT_BRANCH_COLOR\]\$(parse_git_branch)\[$LAMBDA_COLOR\]\$(lambda_or_delta) \[$NO_COLOR\]"

