function fish_prompt --description 'Write out the prompt'
    set -l last_pipestatus $pipestatus
    set -lx __fish_last_status $status # Export for __fish_print_pipestatus.
    set -l normal (set_color normal)
    set -l orange (set_color FFA500)
    set -q fish_color_status
    or set -g fish_color_status red

    # Color the prompt differently when we're root
    set -l color_cwd $fish_color_cwd
    if __git_is_dirty
        set -f suffix "$orange"λ
    else
        set -f suffix "$orange"Δ
    end

    if functions -q fish_is_root_user; and fish_is_root_user
        if set -q fish_color_cwd_root
            set color_cwd $fish_color_cwd_root
        end
        set suffix '#'
    end

    # Write pipestatus
    # If the status was carried over (if no command is issued or if `set` leaves the status untouched), don't bold it.
    set -l bold_flag --bold
    set -q __fish_prompt_status_generation; or set -g __fish_prompt_status_generation $status_generation
    if test $__fish_prompt_status_generation = $status_generation
        set bold_flag
    end
    set __fish_prompt_status_generation $status_generation
    set -l status_color (set_color $fish_color_status)
    set -l statusb_color (set_color $bold_flag $fish_color_status)
    set -l prompt_status (__fish_print_pipestatus "[" "] " "|" "$status_color" "$statusb_color" $last_pipestatus)

    echo -n -s (__prompt_kube_context) (set_color $color_cwd) (prompt_pwd) $normal (fish_vcs_prompt) $normal " " $prompt_status $suffix " "
end


function __git_is_dirty
    set -l diff (git diff --shortstat 2>/dev/null | tail -n1)
    if test -n "$diff"
        return 1
    else
        return 0
    end    
end

function __prompt_kube_context
    if command -q kubectl
        set -f active_context (kubectl config current-context 2>/dev/null)
        set -l kube_color (set_color normal)
        if test "$active_context" = "production"
            set -l kube_color (set_color red)
        end

        if test -n "$active_context"
            echo -n $kube_color
            echo -n "(kube: $active_context) "
        end
    end
end
