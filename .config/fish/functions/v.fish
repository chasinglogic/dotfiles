function v
    if test "$VIRUAL_ENV" != ""
        deactivate
    end

    set -f top_level (git rev-parse --show-toplevel 2>/dev/null)
    if test $status != 0
        set -f top_level (pwd)
    end

    if test -f "$top_level/poetry.lock"
        eval (poetry env activate)
        return 0
    end

    for envdir in "$top_level/env" "$top_level/venv" "$top_level/.venv"
        if test -d $envdir
            source "$envdir/bin/activate.fish"
            return 0
        end
    end

    set -f name (basename $top_level)
    set -f envdir "$top_level/.venv"
    python3 -m venv --prompt $name $envdir
    source "$envdir/bin/activate.fish"
    pip install wheel
end
