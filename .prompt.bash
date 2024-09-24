# shellcheck shell=bash
##########
# PROMPT #
##########

BLACK="$(tput setaf 0)"
RED="$(tput setaf 196)"
GREEN="$(tput setaf 34)"
YELLOW="$(tput setaf 226)"
MAGENTA="$(tput setaf 164)"
PINK="$(tput setaf 201)"
CYAN="$(tput setaf 17)"
WHITE="$(tput setaf 255)"
GREY="$(tput setaf 240)"
RESET="\e[0m"

COMMAND_STATUS_COLOR="$(tput bold)$RED"
HOSTNAME_COLOR="$MAGENTA"
GIT_BRANCH_COLOR="$GREEN"
LAMBDA_COLOR="$YELLOW"
DELTA_COLOR="$YELLOW"

SEPARATOR=" "

function add_sep_if_required {
  if [[ "$1" == "$START" ]]; then
    echo "$1"
  else
    echo "${1}${SEPARATOR}"
  fi
}

function __prompt_command {
  RET="$?"
  PS1=""

  INFO_COLOR="$(tput bold)$WHITE"
  NO_COLOR="$RESET"

  if [[ "$VIRTUAL_ENV_PROMPT" != "" ]]; then
    VENV_NAME=${VIRTUAL_ENV_PROMPT//[() ]/}
    PS1+="\[$INFO_COLOR\][venv: $VENV_NAME]"
  fi

  if [[ $(tput cols) -gt 149 ]]; then
    active_context=$(kubectl config current-context 2>/dev/null)
    if [[ "$active_context" != "" ]]; then
      PS1=$(add_sep_if_required "$PS1")
      PS1+="\[$INFO_COLOR\][kube: ${active_context}]"
    fi

    if [[ "$AWS_PROFILE" != "" ]]; then
      PS1=$(add_sep_if_required "$PS1")
      PS1+="\[$INFO_COLOR\][aws: ${AWS_PROFILE}]"
    fi
  fi

  if [[ -n "$PS1" ]]; then
    PS1+="\n"
  fi

  if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    PS1+="\[$HOSTNAME_COLOR\]\u@\h\[$NO_COLOR\] "
  fi

  if [[ "$RET" != "0" ]]; then
    PS1+="\[$COMMAND_STATUS_COLOR\]!!\[$NO_COLOR\] "
  fi

  PS1+="\[$INFO_COLOR\]\w "

  ref="$(git symbolic-ref HEAD 2>/dev/null)"
  if [[ "$ref" != "" ]]; then
    PS1+="\[$GIT_BRANCH_COLOR\]${ref#refs/heads/} "
  fi

  if [[ "$(git diff --shortstat 2>/dev/null | tail -n1)" != "" ]]; then
    PS1+="\[$DELTA_COLOR\]Δ "
  else
    PS1+="\[$LAMBDA_COLOR\]λ "
  fi

  PS1+="\[$NO_COLOR\]"
  _bash_history_sync
}

PROMPT_COMMAND=__prompt_command
