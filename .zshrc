# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git
)

source $ZSH/oh-my-zsh.sh

# User configuration
export PS1="$FG[004]@$(hostname) %{$reset_color%}$PS1"

export VIRTUALENVWRAPPER_PYTHON=$(which python3)
if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
    source /usr/local/bin/virtualenvwrapper.sh
fi
if [ -f /usr/bin/virtualenvwrapper.sh ]; then
    source /usr/bin/virtualenvwrapper.sh
fi

# Switch projects quickly
function sp() {
    if [[ $1 == "" ]]; then
        cd $(projector list | fzf)
    else
        cd $(projector find $1)
    fi
}

# Activate virtualenvs
function v() {
    if [ -d .git ]; then
        NAME=$(basename $(git rev-parse --show-toplevel))
        workon $NAME
    elif [ -d .venv ]; then
        source .venv/bin/activate
    elif [ -d venv ]; then
        source venv/bin/activate
    else
        ve .venv
        v
    fi
}

function new_sess {
    tmux has-session -t $1
    if [ $? != 0 ]; then
        tmux new-session -d -s $1
    fi

    if [[ $TMUX != "" ]]; then
        tmux switch-client -t $1
    else
        tmux attach -t $1
    fi
}

function t {
    new_sess $(echo $(basename $(pwd)) | sed s/\\./_/g | sed s%/%_%g)
}

function syncpanes() {
    tmux setw synchronize-panes $1
}

### Virtualenvwrapper

export VIRTUALENVWRAPPER_PYTHON="$(which python3)"

if [[ -f /usr/local/bin/virtualenvwrapper.sh ]]; then
    source /usr/local/bin/virtualenvwrapper.sh
fi


if [[ -f ~/.local/bin/virtualenvwrapper.sh ]]; then
    source ~/.local/bin/virtualenvwrapper.sh
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
source $HOME/.profile
