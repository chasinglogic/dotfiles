##########
# PROMPT #
##########

function parse_git_branch {
    ref=$(git symbolic-ref HEAD 2> /dev/null) || return
    echo "${ref#refs/heads/} "
}

function asterisk_if_dirty {
    [[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]] && echo "*"
}

function lambda_or_delta {
    if [[ $(asterisk_if_dirty) == "*" ]]; then
        echo "Δ"
        return
    fi
    echo "λ"
}

function venv_name {
    if [[ ! -z $VIRTUAL_ENV ]]; then
        echo "($(basename $VIRTUAL_ENV)) "
    fi
}

COMMAND_STATUS="%(?..%B%F{red}!! %b%f)"
HOSTNAME="%F{13}%n%F{3}@%F{2}%m%f"
PWD_PROMPT="%F{6}%~%f"
GIT_BRANCH_COLOR="\%1"

setopt prompt_subst

# In case run from bash
unset PS1

# venv assumes PS1 so we will use venv_name to do the prompting
# ourselves
export VIRTUAL_ENV_DISABLE_PROMPT=1

PROMPT="$COMMAND_STATUS\$(venv_name)$HOSTNAME $PWD_PROMPT %F{1}\$(parse_git_branch)%F{3}\$(lambda_or_delta)%f "
