function root --description 'Jump to the root of the current git repo'
    set -l root_dir (git rev-parse --show-toplevel 2>/dev/null)
    if test -n "$root_dir"
        cd $root_dir
    else
        echo "Not in a git repository."
    end
end
