##########
# PROMPT #
##########

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

function kube_context {
    if [[ -x $(which kubectl 2>/dev/null) ]]; then
        current_context=$(kubectl config get-contexts | grep '*' | awk '{ print $2 }')
        if [[ "$current_context" == "" ]]; then
            echo ""
        else
            echo "(kube: $current_context) "
        fi
    fi
}

function aws_profile {
    if [[ "${AWS_PROFILE:-}" != "" ]]; then
        echo "(aws: $AWS_PROFILE) "
    else
        echo ""
    fi
}

COMMAND_STATUS="%(?..%B%F{red}!! %b%f)"
HOSTNAME="%F{blue}%n@%m%f"
PWD_PROMPT="%f%~"

setopt prompt_subst

# In case run from bash
unset PS1

# venv assumes PS1 so we will use venv_name to do the prompting
# ourselves
export VIRTUAL_ENV_DISABLE_PROMPT=1

function set_prompt {
    SHORT_PROMPT=0
    if [[ $COLUMNS -lt 150 ]]; then
        SHORT_PROMPT=1
    fi

    if [[ $SHORT_PROMPT -ne 0 && "$DISABLE_SHORT_PROMPT" == "" ]]; then
        export PROMPT="$PWD_PROMPT %F{green}\$(branch_prompt_info) %f%F{yellow}\$(lambda_or_delta)%f "
    else
        export PROMPT="$COMMAND_STATUS\$(venv_name)\$(aws_profile)\$(kube_context) $PWD_PROMPT %F{green}\$(branch_prompt_info) %F{yellow}\$(lambda_or_delta)%f "
    fi
}

set_prompt

trap 'set_prompt' SIGWINCH
