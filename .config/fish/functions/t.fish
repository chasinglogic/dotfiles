function t
    set -l session_name (basename (pwd))
    if test -d ".git"
        zellij --layout dev attach --create-background $session_name
    else
        zellij attach --create-background $session_name
    end

    if test -n "$ZELLIJ"
        echo "Created session: $session_name"
    else
        zellij attach $session_name
    end
end
