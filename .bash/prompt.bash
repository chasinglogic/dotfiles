ORANGE=$(tput setaf 166)
RED=$(tput setaf 160)
VIOLET="\e[35m"
BLUE=$(tput setaf 33)
CYAN=$(tput setaf 37)
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

PS1="\[$RED\]\$(last_command_status)\[$VIOLET\]@$HOSTNAME\[$CYAN\] \$(pretty_pwd) \[$CYAN\]\$(parse_git_branch)\[$ORANGE\]\$(lambda_or_delta) \[$NO_COLOR\]"

