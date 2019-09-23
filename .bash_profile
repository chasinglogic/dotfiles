source ~/.profile

if [[ -n $TMUX ]]; then
    source ~/.bashrc
fi

export PATH="$HOME/.cargo/bin:$PATH"
