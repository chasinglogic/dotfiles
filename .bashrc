export HISTCONTROL=ignoredups:erasedups
shopt -s histappend

shopt -s checkwinsize

shopt -s globstar

if [[ -f ~/.bashrc_extras ]]; then
    source ~/.bashrc_extras
fi

source ~/.profile

function dotfiles() {
    cd $(dfm where)
}

alias ll="ls -alF"
alias la="ls -a"
alias l="ls -CF"

alias cd..="cd .."
alias cdc="cd $HOME/Code"
alias cdw="cd $HOME/Work"

alias apt="sudo apt"
alias zyp="sudo zypper"
alias dnf="sudo dnf"
alias pca="pacaur"
alias pac="sudo pacman"
alias pacman="sudo pacman"

alias g="git"
alias gc="git commit -v"
alias ga="git add"
alias gb="git branch"
alias gp="git push"
alias gpl="git pull"
alias gck="git checkout"
alias gcp="git cherry-pick"
alias gst="git status"
alias gru="git remote update"

alias ec2="aws ec2"
alias s3="aws s3"
alias tf="terraform"

alias p="python"
alias p3="python3"
alias ve="python3 -m venv"
alias venv="python3 -m venv"

function v() {
    if [ -d .git ]; then
        NAME=$(basename $(git rev-parse --show-toplevel))
        workon $NAME
    elif [ -d .venv ]; then
        source .venv/bin/activate
    elif [ -d venv ]; then
        source venv/bin/activate
    else
        ve .venv
        v
    fi
}

### Virtualenvwrapper

export VIRTUALENVWRAPPER_PYTHON="$(which python3)"

if [[ -f /usr/local/bin/virtualenvwrapper.sh ]]; then
    source /usr/local/bin/virtualenvwrapper.sh
fi


if [[ -f ~/.local/bin/virtualenvwrapper.sh ]]; then
    source ~/.local/bin/virtualenvwrapper.sh
fi


### My custom functions

function sp() {
    PROJ_NAME=""
    if [[ -n $1 ]]; then
        PROJ_NAME="$1"
    else
        echo -n "Project name: "
        read PROJ_NAME
    fi

    cd $(projector find $PROJ_NAME)
}

function new_sess {
    tmux has-session -t $1
    if [ $? != 0 ]; then
        tmux new-session -d -s $1
    fi

    if [[ $TMUX != "" ]]; then
        tmux switch-client -t $1
    else
        tmux attach -t $1
    fi
}

function t {
    new_sess $(echo $(basename $(pwd)) | sed s/\\./_/g | sed s%/%_%g)
}

function syncpanes() {
    tmux setw synchronize-panes $1
}

function tssh() {
    HOST_NAME=${@: -1}
    SAFE_NAME=${HOST_NAME//./-}
    SAFE_NAME=${SAFE_NAME//@/-at-}

    tmux has-session -t $SAFE_NAME
    if [ $? != 0 ]; then
        tmux new-session -d -s $SAFE_NAME "ssh $@"
    fi

    if [[ $TMUX != "" ]]; then
        tmux switch-client -t $SAFE_NAME
    else
        tmux attach -t $SAFE_NAME
    fi
}

if [ -t 1 ]; then

ORANGE=$(tput setaf 166)
RED=$(tput setaf 160)
VIOLET="\e[35m"
BLUE=$(tput setaf 33)
CYAN=$(tput setaf 37)
NO_COLOR="\e[0m"

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

function last_command_status {
    if [[ $? == "0" ]]; then
        return
    fi

    echo "!! "
}

# Make the pwd look pretty
function pretty_pwd {
    dir=`pwd | awk -F\/ '{print $(NF-1),$(NF)}' | sed 's/ /\\//'`
    echo "$dir"
}

PS1="\[$RED\]\$(last_command_status)\[$VIOLET\]@$HOSTNAME\[$BLUE\] \$(pretty_pwd) \[$CYAN\]\$(parse_git_branch)\[$ORANGE\]\$(lambda_or_delta) \[$NO_COLOR\]"
fi
