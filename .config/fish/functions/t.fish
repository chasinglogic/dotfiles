function t
    set -l session_name (basename (pwd))

    if ! tmux has-session -t $session_name
        if test -d .git
            tmux new-session -n 'nvim' -d -s $session_name 'nvim .'
            tmux new-window
            tmux select-window -t 1
        else
            tmux new-session -d -s $session_name 
        end

    end

    if test -n "$TMUX"
        tmux switch-client -t $session_name
    else
        tmux attach-session -t $session_name
    end
end
