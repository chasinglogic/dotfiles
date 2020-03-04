# A colon-separated list of values controlling how commands are saved on the
# history list.  If the list  of  values  includes ignorespace,  lines  which
# begin  with  a space character are not saved in the history list.  A value of
# ignoredups causes lines matching the previous history entry to not be saved.
# A value of ignoreboth is  shorthand  for  ignorespace  and  ig‐ noredups.  A
# value of erasedups causes all previous lines matching the current line to be
# removed from the history list be‐ fore that line is saved.  Any value not in
# the above list is ignored.  If HISTCONTROL is unset, or does not include a
# valid value,  all  lines  read by the shell parser are saved on the history
# list, subject to the value of HISTIGNORE.  The second and subsequent lines of
# a multi-line compound command are not tested, and are added to the history
# regardless of the  value of HISTCONTROL.
export HISTCONTROL=ignoredups:erasedups

# If the histappend shell option is enabled (see the description of shopt under
# SHELL BUILTIN COMMANDS below), the lines are  appended to the history file,
# otherwise the history file is overwritten.
shopt -s histappend

# If  set,  the pattern ** used in a pathname expansion context will match all
# files and zero or more directories and subdirectories.  If the pattern is
# followed by a /, only directories and subdirectories match.
shopt -s globstar

# If  the  extglob  shell option is enabled using the shopt builtin, several extended pattern matching operators are recognized.  In
# the following description, a pattern-list is a list of one or more patterns separated by a |.  Composite patterns  may  be  formed
# using one or more of the following sub-patterns:
#
#        ?(pattern-list)
#               Matches zero or one occurrence of the given patterns
#        *(pattern-list)
#               Matches zero or more occurrences of the given patterns
#        +(pattern-list)
#               Matches one or more occurrences of the given patterns
#        @(pattern-list)
#               Matches one of the given patterns
#        !(pattern-list)
#               Matches anything except one of the given patterns
#
# Complicated  extended  pattern  matching  against  long strings is slow, especially when the patterns contain alternations and the
# strings contain multiple matches.  Using separate matches against shorter strings, or using arrays of strings instead of a  single
# long string, may be faster.
shopt -s extglob

# If set, bash replaces directory names with the results of word expansion when
# performing filename completion.  This changes the contents of the readline
# editing buffer.  If not set, bash attempts to preserve what the user typed.
#
# Note: this is enabled by default on most distributions I think but might as
# well make sure it's always on.
shopt -s direxpand

# If set, bash attempts spelling correction on directory names during word
# completion if the directory name initially supplied does not exist.
#
# Note: this only works on paths with at least one level of dir, i.e. `cd
# mispelled` won't autocorrect but `cd mispelled/somedir` will.
shopt -s dirspell

# autocd  If  set,  a  command name that is the name of a directory is executed
# as if it were the argument to the cd command. This option is only used by
# interactive shells.
shopt -s autocd

# Load the profile if it's not loaded yet. My .profile is idempotent so it can
# be sourced multiple times safely without causing weird environments.
source $HOME/.profile

alias ll="ls -alF"
alias la="ls -a"
alias l="ls -CF"
alias em="emacs -nw"

alias cd..="cd .."
alias cdc="cd $HOME/Code"
alias cdw="cd $HOME/Work"

alias g="git"
alias tf="terraform"

alias tm="tmux"

alias venv="python3 -m venv"

#############
# FUNCTIONS #
#############

function dotfiles() {
    cd $(dfm where)
}

function sp() {
    if [[ $1 == "" ]]; then
        cd $(projector list | fzf)
    else
        cd $(projector find $1)
    fi

    if [[ -d $(pwd)/.git ]]; then
        NAME=$(basename $(git rev-parse --show-toplevel))
        if [[ -d ~/.virtualenvs/$NAME ]]; then
            workon $NAME
        fi
    fi
}


function source_if_exists() {
    [ -f $1 ] && source $1
}

function v() {
    NAME=$(basename $(git rev-parse --show-toplevel 2>/dev/null))

    if [ -d .git ] && [ -d $HOME/.virtualenvs/$NAME ]; then
        workon $NAME
    elif [ -d .venv ]; then
        source .venv/bin/activate
    elif [ -d venv ]; then
        source venv/bin/activate
    elif [ -d .git ] && [ -d $HOME/.virtualenvs ]; then
        mkvirtualenv $NAME
    else
        venv .venv
        v
    fi
}

function t {
    tmux new-session -A -s $(pwd | awk -F\/ '{print $(NF)}')
}

##########
# PROMPT #
##########


COMMAND_STATUS_COLOR="\$(tput bold)\$(tput setaf 5)"
HOSTNAME_COLOR="\$(tput setaf 2)"
PWD_COLOR="\$(tput setaf 6)"
GIT_BRANCH_COLOR="\$(tput setaf 1)"
LAMBDA_COLOR="\$(tput setaf 3)"
USERNAME_COLOR="\$(tput setaf 13)"
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

PS1="\[$COMMAND_STATUS_COLOR\]\$(last_command_status)\[$USERNAME_COLOR\]\u\[$LAMBDA_COLOR\]@\[$HOSTNAME_COLOR\]\H\[$PWD_COLOR\] \w \[$GIT_BRANCH_COLOR\]\$(parse_git_branch)\[$LAMBDA_COLOR\]\$(lambda_or_delta) \[$NO_COLOR\]"

source_if_exists $HOME/.fzf.bash
source_if_exists $HOME/.bashrc_extras
