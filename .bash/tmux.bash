# TMUX related bash configs

alias tm="tmux"

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
    tmux new-session -A -s $(pwd | awk -F\/ '{print $(NF)}')
}

function ts {
    if [[ $TMUX != "" ]]; then
        tmux switch-client -t $(tmux list-sessions -F "#S" | fzf)
        return
    fi

    tmux attach -t $(tmux list-sessions -F "#S" | fzf)
}

function syncpanes() {
    tmux setw synchronize-panes $1
}

function tssh() {
    SAFE_NAME=${1//./}
    echo "Name $SAFE_NAME"
    tmux has-session -t $SAFE_NAME
    if [ $? != 0 ]; then
        tmux new-session -d -s $SAFE_NAME "ssh $@"
    fi

    if [[ $TMUX != "" ]]; then
        tmux switch-client -t $SAFE_NAME
    else
        tmux attach -t $SAFE_NAME
    fi
}
