#############
# FUNCTIONS #
#############

function pyenv_activate() {
  eval "$(pyenv init -)"
}

function dotfiles() {
  cd $DOTFILES
}

function page() {
  $@ | less
}

function redact() {
  $@ 1>/dev/null 2>/dev/null
}

function st() {
  sess=""
  if [[ -n $1 ]]; then
    sess=$(tmux list-session -F "#S" | grep -i $1)
  else
    sess=$(tmux list-session -F "#S" | fzf)
  fi

  if [[ -n $TMUX ]]; then
    tmux switch-client -t $sess
  else
    tmux attach-session -t $sess
  fi
}

function sp() {
  PROJECT=""
  if [[ $1 == "" ]]; then
    PROJECT=$(projector list | fzf)
  elif [[ $1 == "home" ]]; then
    PROJECT=$HOME
  else
    PROJECT=$(projector find "(?i)$1")
  fi

  if [[ $? != 0 ]]; then
    EXITCODE=$?
    echo $PROJECT
    return $EXITCODE
  fi

  cd $PROJECT
  if [ -f .env ]; then
    source .env
  fi
}

function v() {
  TOP_LEVEL=$(git rev-parse --show-toplevel 2>/dev/null)
  if [[ $? != 0 ]]; then
    TOP_LEVEL=$(pwd)
  fi

  NAME=$(basename $TOP_LEVEL)
  ENVDIR="$TOP_LEVEL/env"
  if [[ ! -d $ENVDIR ]]; then
    python3 -m venv --prompt $NAME $ENVDIR
  fi

  if [[ -n $(env | grep 'VIRTUAL_ENV=') ]]; then
    deactivate
  fi

  source $ENVDIR/bin/activate
}

function nv() {
  if test $# -gt 0; then
    env $VIM_PROG "$@"
  elif test -f Session.vim; then
    env $VIM_PROG -S
  else
    env $VIM_PROG -c Obsession .
  fi
}

function t() {
  SESS_NAME=${PWD##*/}
  tmux has-session -t $SESS_NAME
  if [ $? -ne 0 ]; then
    tmux new-session -s $SESS_NAME -d
  fi

  if [[ -n $TMUX ]]; then
    tmux switch-client -t $SESS_NAME
  else
    tmux attach-session -t $SESS_NAME
  fi
}
