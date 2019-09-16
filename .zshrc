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


# Source my "sub bashrc" scripts they work for ZSH as well.
if [[ -d ~/.bash ]]; then
    for f in ~/.bash/*; do
        source $f
    done
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
source $HOME/.profile
