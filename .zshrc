# This makes TRAMP from Emacs work when ZSH is the default shell.
#
# Otherwise TRAMP hangs forever waiting for a prompt that never shows
# because of the regex.
if [[ $TERM == "dumb" ]]; then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    if whence -w precmd >/dev/null; then
        unfunction precmd
    fi
    if whence -w preexec >/dev/null; then
        unfunction preexec
    fi
    export PS1="$ "
    return
fi

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
plugins=()

source $ZSH/oh-my-zsh.sh

# User configuration
export PS1="$FG[004]@$(hostname) %{$reset_color%}$PS1"

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

function ts {
    SESS=$(tmux list-sessions | grep $1 | awk '{ print $1 }' | sed 's/:$//')
    if [[ -z $SESS ]]; then
        if [[ -x $(which projector 2>/dev/null) ]]; then
            PROJ=$(projector find $1)
            if [[ -n $PROJ ]]; then
                cd $PROJ
                t
            fi
        fi

        echo "Session not found. Available sessions are:"
        tmux list-sessions
        return
    fi

    if [[ -n $TMUX ]]; then
        tmux switch-client -t $SESS
    else
        tmux attach -t $SESS
    fi
}

function syncpanes() {
    tmux setw synchronize-panes $1
}

function dotfiles() {
    cd $(dfm where)
}

function et() {
    emacsclient --tty -a 'vi' $@
}

function ec() {
    emacsclient -a 'vi' $@
}

### Virtualenvwrapper

export VIRTUALENVWRAPPER_PYTHON="$(which python3)"

if [[ -f /usr/local/bin/virtualenvwrapper.sh ]]; then
    source /usr/local/bin/virtualenvwrapper.sh
fi


if [[ -f $HOME/.local/bin/virtualenvwrapper.sh ]]; then
    source $HOME/.local/bin/virtualenvwrapper.sh
fi


if [[ -f /usr/share/virtualenvwrapper/virtualenvwrapper.sh ]]; then
    source /usr/share/virtualenvwrapper/virtualenvwrapper.sh
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
source $HOME/.profile

### Aliases
if [[ -x $(which ssh) ]]; then
    if [[ $TERM == "xterm-24bit" ]]; then
        alias ssh="TERM=xterm $(which ssh)"
    elif [[ $TERM == "xterm-kitty" ]]; then
        alias ssh="kitty +kitten ssh"
    fi
fi

if [[ -x $(which nvim 2>/dev/null) ]]; then
    alias vim="nvim"
fi

alias nv="nvim"
alias cdc="cd $HOME/Code"
alias g="git"
