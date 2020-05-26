#############
# FUNCTIONS #
#############

function dotfiles() {
  cd $(dfm where)
}

function et() {
    emacsclient -nw $@
}

function ec() {
    et $@
}

function page() {
  $@ | less
}

function tmux_session_name() {
  if [ -z $1 ]; then
    return "default"
  fi

  echo $1 |\
  awk -F\/ '{print $(NF)}' |\
  sed 's/\./-/g'
}

function tmux_session() {
  SESS_NAME=$1
  if [[ -z $(tmux list-sessions | grep "^$SESS_NAME") ]]; then
    echo "Creating $SESS_NAME"
    tmux new-session -s $SESS_NAME -d
  else
    echo "$SESS_NAME already exists."
  fi

  if [[ -n $TMUX ]]; then
    tmux switch-client -t $SESS_NAME
  else
    tmux attach-session -t $SESS_NAME
  fi
}

function sp() {
  PROJECT=""
  if [[ $1 == "" ]]; then
    PROJECT=$(projector list | fzf)
  elif [[ $1 == "home" ]]; then
    PROJECT=$HOME
  else
    PROJECT=$(projector find $1)
  fi

  SESS_NAME=$(tmux_session_name $PROJECT)
  CURRENT_SESSION=""
  if [[ -n $TMUX ]]; then
    CURRENT_SESSION=$(tmux list-sessions | grep '(attached)' | awk '{ print $1 }' | sed 's/:$//')
  fi

  cd $PROJECT
  if [[ "$SESS_NAME" != "$CURRENT_SESSION" ]]; then
    tmux_session $SESS_NAME
    return
  fi

  if [[ -d $(pwd)/.git ]]; then
    NAME=$(basename $(git rev-parse --show-toplevel))
    if [[ -d ~/.virtualenvs/$NAME ]]; then
      workon $NAME
    fi
  fi
}

function v() {
  if [[ -d .git ]]; then
    NAME=$(basename $(git rev-parse --show-toplevel 2>/dev/null))
  else
    NAME=$(basename $(pwd))
  fi

  if [[ -d $HOME/.virtualenvs/$NAME ]]; then
    workon $NAME
  else
    mkvirtualenv $NAME
  fi
}

function t() {
  SESS_NAME=$(tmux_session_name $(pwd))
  tmux_session $SESS_NAME
}

export VIM_PROG=vim
if [[ -x $(which nvim) ]]; then
  export VIM_PROG=nvim
fi

function nv() {
  if test $# -gt 0; then
    env $VIM_PROG "$@"
  elif test -f Session.vim; then
    env $VIM_PROG -S
  else
    env $VIM_PROG -c Obsession
  fi
}

# Poor man's virtualenvwrapper

function checkvenvdir() {
  if [[ ! -d $HOME/.virtualenvs ]]; then
    mkdir -p $HOME/.virtualenvs
  fi
}

function workon() {
  if [[ -d $HOME/.virtualenvs/$1 ]]; then
    source $HOME/.virtualenvs/$1/bin/activate
  fi
}

function mkvirtualenv() {
  checkvenvdir
  new_venv_path="$HOME/.virtualenvs/$1"
  if [[ -d $new_venv_path ]]; then
    echo "Virtualenv $1 already exists!"
    return 1
  fi

  python3 -m venv $new_venv_path
  workon $1
}
