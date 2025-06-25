function sp
    if test "$argv[1]" = ""
        set -f project (projector list | fzf)
    else
        set -f project (projector find "(?i)$argv[1]")
    end

    set -l exit_code $status
    if test $exit_code != 0
        echo $project
        return $exit_code
    end

    cd $project
end
