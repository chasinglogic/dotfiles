function fish_user_key_bindings
    # Start with the default Emacs-style bindings so we're additive.
    fish_default_key_bindings

    # Match Bash behavior for word deletion (Alt+Backspace / Alt+D).
    bind alt-backspace backward-kill-word
    bind alt-d kill-word

    # Bash-style edit/yank helpers.
    bind \cx\ce edit_command_buffer
    bind \ey yank-pop
end
