function t
    if test -z "$SHELL"
        set -gx SHELL (which fish)
    end

    if test -n "$argv[1]"
        set -f session_name $argv[1]
    else
        set -f session_name (basename (pwd))
    end

    echo "Starting session: $session_name"

    if ! tmux has-session -t $session_name
        tmux new-session -d -s $session_name
    end

    if test -n "$TMUX"
        tmux switch-client -t $session_name
    else
        tmux attach-session -t $session_name
    end
end
