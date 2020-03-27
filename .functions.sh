#############
# FUNCTIONS #
#############

function dotfiles() {
    cd $(dfm where)
}

function sp() {
    if [[ $1 == "" ]]; then
        cd $(projector list | fzf)
    else
        cd $(projector find $1)
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

function t {
    tmux new-session -A -s $(pwd | awk -F\/ '{print $(NF)}')
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
