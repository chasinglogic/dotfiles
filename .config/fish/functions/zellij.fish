function t
    set -l session_name (basename (pwd))
    zellij attach --create-background $session_name

    if test -n "$ZELLIJ"
        echo "Created session: $session_name"
    else
        zellij attach $session_name
    end
end
