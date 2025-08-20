function sp
    if test "$argv[1]" = ""
        set -f project (projector list | fzf)
    else
        set -f project (projector find "(?i)$argv[1]")
    end

    if test $status != 0
        echo $project
        return $exit_code
    end

    cd $project
end
