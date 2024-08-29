# shellcheck shell=bash
##########
# PROMPT #
##########

BLACK="$(tput setaf 7)"
RED="$(tput setaf 196)"
GREEN="$(tput setaf 40)"
YELLOW="$(tput setaf 226)"
MAGENTA="$(tput setaf 163)"
PINK="$(tput setaf 200)"
CYAN="$(tput setaf 27)"
WHITE="$(tput setaf 0)"
GREY="$(tput setaf 8)"
ORANGE="$(tput setaf 214)"

CONTEXT_INFO_COLOR="$BLACK"
COMMAND_STATUS_COLOR="$(tput bold)$RED"
HOSTNAME_COLOR="$MAGENTA"
GIT_BRANCH_COLOR="$GREEN"
LAMBDA_COLOR="$ORANGE"
DELTA_COLOR="$ORANGE"
NO_COLOR="\e[0m"

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

  if [[ "$VIRTUAL_ENV_PROMPT" != "" ]]; then
    VENV_NAME=${VIRTUAL_ENV_PROMPT//[() ]/}
    PS1+="\[$CONTEXT_INFO_COLOR\][venv: $VENV_NAME]"
  fi

  if [[ $(tput cols) -gt 149 ]]; then
    active_context=$(kubectl config current-context 2>/dev/null)
    if [[ "$active_context" != "" ]]; then
      PS1=$(add_sep_if_required "$PS1")
      PS1+="\[$CONTEXT_INFO_COLOR\][kube: ${active_context}]"
    fi

    if [[ "$AWS_PROFILE" != "" ]]; then
      PS1=$(add_sep_if_required "$PS1")
      PS1+="\[$CONTEXT_INFO_COLOR\][aws: ${AWS_PROFILE}]"
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

  PS1+="\[$CONTEXT_INFO_COLOR\]\w "

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
