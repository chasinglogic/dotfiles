source ~/.profile

if [[ -n $TMUX ]]; then
    source ~/.bashrc
fi

source "$HOME/.cargo/env"
