# shellcheck shell=bash
##########
# PROMPT #
##########

BLACK="$(tput setaf 0)"
RED="$(tput setaf 196)"
GREEN="$(tput setaf 34)"
YELLOW="$(tput setaf 226)"
MAGENTA="$(tput setaf 128)"
PINK="$(tput setaf 165)"
CYAN="$(tput setaf 17)"
WHITE="$(tput setaf 255)"
GREY="$(tput setaf 240)"
ORANGE="$(tput setaf 209)"
RESET="\e[0m"

NO_COLOR="${RESET}"
COMMAND_STATUS_COLOR="$(tput bold)${RED}"
LAMBDA_COLOR="${RESET}${YELLOW}"
DELTA_COLOR="${RESET}${YELLOW}"
INFO_COLOR="$(tput bold)${WHITE}"

SEPARATOR=" "

function add_sep_if_required {
  if [[ "$1" == "${START}" ]]; then
    echo "$1"
  else
    echo "${1}${SEPARATOR}"
  fi
}

function __kube_context_prompt {
  active_context=$(kubectl config current-context 2>/dev/null)
  if [[ "$active_context" == "production" ]]; then
    echo "${RED}${active_context}"
  else
    echo "${active_context}"
  fi
}

function __git_branch_prompt {
  ref="$(git symbolic-ref HEAD 2>/dev/null)"
  branch=${ref#refs/heads/}
  if [[ "$branch" == "main" || "$branch" == "master" ]]; then
    echo "${RED}${branch}"
  else
    echo "${branch}"
  fi
}

function __prompt_command {
  RET="$?"
  PS1=""

  function __info_segment {
    PS1=$(add_sep_if_required "${PS1}")
    PS1+="\[${INFO_COLOR}\](${1}: ${2}\[${INFO_COLOR}\])"
  }

  if [[ -n "${VIRTUAL_ENV_PROMPT}" ]]; then
    VENV_NAME=${VIRTUAL_ENV_PROMPT//[() ]/}
    __info_segment "venv" "$VENV_NAME"
  fi

  active_context=$(__kube_context_prompt)
  if [[ -n "$active_context" ]]; then
    __info_segment "kube" "$active_context"
  fi

  if [[ "${RET}" != "0" ]]; then
    PS1+="\[${COMMAND_STATUS_COLOR}\]!!\[${NO_COLOR}\] "
  fi

  PS1=$(add_sep_if_required "$PS1")
  PS1+="\w "

  branch=$(__git_branch_prompt)
  if [[ -n "$branch" ]]; then
    PS1+="$branch "
  fi

  if [[ "$(git diff --shortstat 2>/dev/null | tail -n1)" != "" ]]; then
    PS1+="\[${DELTA_COLOR}\]Δ "
  else
    PS1+="\[${LAMBDA_COLOR}\]λ "
  fi

  PS1+="\[${NO_COLOR}\]"
  _bash_history_sync
}

PROMPT_COMMAND=__prompt_command
