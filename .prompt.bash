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
NO_COLOR="\e[0m"

COMMAND_STATUS_COLOR="$(tput bold)$RED"
HOSTNAME_COLOR="$NO_COLOR"
GIT_BRANCH_COLOR="$NO_COLOR"
LAMBDA_COLOR="$YELLOW"
DELTA_COLOR="$YELLOW"
DEFAULT_COLOR="$NO_COLOR"

STACK_DIR=""
STACK_CACHE=""

function get_stack {
  if [[ ! -f "Pulumi.yaml" && -z "$STACK_DIR" ]]; then
     return
  fi

  if [[ "$STACK_DIR" == $(pwd) ]]; then
    if [[ -z "$STACK_CACHE" ]]; then
      STACK_CACHE=$(pulumi stack --show-name)
    fi
  else
    STACK_DIR=$(pwd)
    STACK_CACHE=$(pulumi stack --show-name)
  fi
}

function __prompt_command {
    RET="$?"
    PS1=""

    if [[ "$RET" != "0" ]]; then
      PS1+="\[$COMMAND_STATUS_COLOR\]!!\[$DEFAULT_COLOR\] "
    fi

    if [[ "$VIRTUAL_ENV_PROMPT" != "" ]]; then
      PS1+="\[$DEFAULT_COLOR\]$VIRTUAL_ENV_PROMPT"
    fi

    if [[ $(tput cols) -gt 149 ]]; then
      active_context=$(kubectl config current-context 2>/dev/null)
      if [[ "$active_context" != "" ]]; then
        PS1+="\[$DEFAULT_COLOR\](kube: ${active_context}) "
      fi

      if [[ "$AWS_PROFILE" != "" ]]; then
          PS1+="\[$DEFAULT_COLOR\](aws: ${AWS_PROFILE}) "
      fi

      if [[ -f "Pulumi.yaml" ]]; then
        get_stack
        PS1+="\[$DEFAULT_COLOR\](stack: ${STACK_CACHE}) "
      fi
    fi

    PS1+="\[$DEFAULT_COLOR\]\w "

    ref="$(git symbolic-ref HEAD 2> /dev/null)"
    if [[ "$ref" != "" ]]; then
        PS1+="\[$GIT_BRANCH_COLOR\]${ref#refs/heads/} "
    fi

    if [[ "$(git diff --shortstat 2> /dev/null | tail -n1)" != "" ]]; then
        PS1+="\[$DELTA_COLOR\]Δ "
    else
        PS1+="\[$LAMBDA_COLOR\]λ "
    fi

    PS1+="\[$NO_COLOR\]"
    _bash_history_sync
}

PROMPT_COMMAND=__prompt_command
