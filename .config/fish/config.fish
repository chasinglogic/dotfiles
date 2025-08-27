if status is-interactive
    if command -q mise
        mise activate fish | source
    end

    if command -q fzf
        fzf --fish | source
    end

    if command -q direnv
        direnv hook fish | source
    end
end
