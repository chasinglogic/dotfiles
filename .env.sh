##############
# ENV setup. #
##############

if [[ -n $BASH_VERSION ]]; then
    export SHELLNAME="bash";
elif [[ -n $ZSH_VERSION ]]; then
    export SHELLNAME="zsh";
else
    export SHELLNAME="sh";
fi

source_if_exists $HOME/.${SHELLNAME}rc.local
# Load the profile if it's not loaded yet. My .profile is idempotent so it can
# be sourced multiple times safely without causing weird environments.
source_if_exists $HOME/.profile
source_if_exists $HOME/.fzf.${SHELLNAME}
source_if_exists $HOME/.prompt.$SHELLNAME
