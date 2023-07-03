##########
# PROMPT #
##########

BLACK="$(tput setaf 0)"
RED="$(tput setaf 1)"
GREEN="$(tput setaf 2)"
YELLOW="$(tput setaf 3)"
MAGENTA="$(tput setaf 4)"
PINK="$(tput setaf 5)"
CYAN="$(tput setaf 6)"
WHITE="$(tput setaf 7)"
GREY="$(tput setaf 8)"

COMMAND_STATUS_COLOR="$(tput bold)$RED"
HOSTNAME_COLOR="$MAGENTA"
GIT_BRANCH_COLOR="$GREEN"
LAMBDA_COLOR="$YELLOW"
DELTA_COLOR="$YELLOW"
NO_COLOR="\e[0m"

function __prompt_command {
    RET="$?"
    PS1=""

    last_command_status=""
    if [[ "$RET" != "0" ]]; then
        last_command_status="$COMMAND_STATUS_COLOR!! "
    fi

    kube_context=""
    if [[ -x $(which kubectl 2>/dev/null) ]]; then
        kube_context="$WHITE(kube: $(kubectl config get-contexts | grep '*' | awk '{ print $2 }'))"
    fi

    lambda_or_delta="${LAMBDA_COLOR}λ"
    if [[ "$(git diff --shortstat 2> /dev/null | tail -n1)" != "" ]]; then
        lambda_or_delta="${DELTA_COLOR}Δ"
    fi

    git_branch=""
    ref=$(git symbolic-ref HEAD 2> /dev/null) || ""
    if [[ "$ref" != "" ]]; then
        git_branch="${GIT_BRANCH_COLOR}${ref#refs/heads/}"
    fi

    PS1="${last_command_status}${kube_context}\[$HOSTNAME_COLOR\]\u@\H\[$NO_COLOR\] \w ${git_branch} ${lambda_or_delta}\[$NO_COLOR\] "
}

PROMPT_COMMAND=__prompt_command
