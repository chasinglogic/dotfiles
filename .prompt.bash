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
WHITE="$(tput setaf 15)"
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

    if [[ "$RET" != "0" ]]; then
        PS1+="\[$COMMAND_STATUS_COLOR\]!!\[$NO_COLOR\] "
    fi

    if [[ $(tput cols) -gt 149 ]]; then
      active_context=$(kubectl config current-context 2>/dev/null)
      if [[ "$active_context" != "" ]]; then
        PS1+="\[$WHITE\](kube: $active_context) "
      fi

      if [[ "$AWS_PROFILE" != "" ]]; then
          PS1+="$WHITE(aws: $AWS_PROFILE) "
      fi

      PS1+="\[$HOSTNAME_COLOR\]\u@\H "
    fi

    PS1+="\[$WHITE\]\w "

    ref=$(git symbolic-ref HEAD 2> /dev/null) || ""
    if [[ "$ref" != "" ]]; then
        PS1+="\[$GIT_BRANCH_COLOR\]${ref#refs/heads/} "
    fi

    if [[ "$(git diff --shortstat 2> /dev/null | tail -n1)" != "" ]]; then
        PS1+="\[$DELTA_COLOR\]Δ "
    else
        PS1+="\[$LAMBDA_COLOR\]λ "
    fi

    PS1+="\[$NO_COLOR\]"
}

PROMPT_COMMAND=__prompt_command
