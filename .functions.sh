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


function source_if_exists() {
    [ -f $1 ] && source $1
}

function v() {
    NAME=$(basename $(git rev-parse --show-toplevel 2>/dev/null))

    if [ -d .git ] && [ -d $HOME/.virtualenvs/$NAME ]; then
        workon $NAME
    elif [ -d .venv ]; then
        source .venv/bin/activate
    elif [ -d venv ]; then
        source venv/bin/activate
    elif [ -d .git ] && [ -d $HOME/.virtualenvs ]; then
        mkvirtualenv $NAME
    else
        venv .venv
        v
    fi
}

function t {
    tmux new-session -A -s $(pwd | awk -F\/ '{print $(NF)}')
}
