if status is-interactive
    # Commands to run in interactive sessions can go here
end

if command -q mise
    mise activate fish | source
end
