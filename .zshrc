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

# Do bash-style backwards deletion, treat dir sep, dot, and other common
# characters as words for readline bindings 
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

source $HOME/.functions.sh
source $HOME/.profile
source_if_exists $HOME/.env.sh
source_if_exists $HOME/.aliases.sh

HISTSIZE=5000               # How many lines of history to keep in memory
HISTFILE=~/.zsh_history     # Where to save history to disk
SAVEHIST=5000               # Number of history entries to save to disk
HISTDUP=erase               # Erase duplicates in the history file
setopt    appendhistory     # Append history to the history file (no overwriting)
setopt    sharehistory      # Share history across terminals
setopt    incappendhistory  # Immediately append to the history file, not just when a term is killed
eval "$(pyenv init -)"
