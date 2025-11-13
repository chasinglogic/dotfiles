set -g KEY_COLOR (set_color 89DCEB)
set -g NORMAL (set_color normal)

function fish_prompt --description 'Write out the prompt'
    set -l last_pipestatus $pipestatus
    set -lx __fish_last_status $status # Export for __fish_print_pipestatus.
    set -l orange (set_color FAB387)
    set -q fish_color_status
    or set -g fish_color_status red

    if __git_is_dirty
        set -f suffix "$orange"λ
    else
        set -f suffix "$orange"Δ
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

    set -l status_line "$(set_color $fish_color_cwd)$(prompt_pwd) $(__prompt_git_branch) $(__prompt_kube_context) \n"

    echo -e -n -s $status_line $prompt_status (__prompt_host) $suffix " "
end

function status_segment
    set -l key $argv[1]
    set -l value $argv[2]

    echo -s -n -e $KEY_COLOR $key "=" $NORMAL $value " " $NORMAL
end

function __prompt_host
    set -l host_color (set_color E64553)
    set -f host (hostname | sed 's/.local//')
    if test -n "$SSH_CLIENT"
        set -f host "$USER@$host"
    else
        set -f host "@$host"
    end

    echo -n "$host_color$host "
end

function __git_is_dirty
    set -l diff (git diff --shortstat 2>/dev/null | tail -n1)
    if test -n "$diff"
        return 1
    else
        return 0
    end
end

function __prompt_git_branch
    set -l branch (fish_vcs_prompt '%s')
    if test "$branch" != ''
        status_segment vcs $branch 
    end
end

function __prompt_kube_context
    if command -q kubectl
        set -f active_context (kubectl config current-context 2>/dev/null)
        set -f kube_color (set_color normal)
        if test "$active_context" = production
            set -f kube_color (set_color red)
        end

        if test -n "$active_context"
            status_segment kube $kube_color$active_context
        end
    end
end
