function st
    if test -z "$ZELLIJ"
        set -l selected_session (zellij list-sessions --no-formatting | fzf | awk '{ print $1 }')
        zellij attach $selected_session
    else
        echo "Cannot run while in zellij because there is no command to switch the active session."
        return 1
    end
end
