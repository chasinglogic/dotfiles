# Handy python things in bash

alias venv="python3 -m venv"

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

source_if_exists /usr/local/bin/virtualenvwrapper.sh
source_if_exists $HOME/.local/bin/virtualenvwrapper.sh
