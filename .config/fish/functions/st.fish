function st
    set -l session_name (tmux list-sessions | fzf | awk '{ print $1 }')

    if test -n $TMUX
        tmux switch-client -t $session_name
    else
        tmux attach-session -t $session_name
    end
end
