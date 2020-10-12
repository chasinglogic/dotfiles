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
