alias ll="ls -alF"
alias la="ls -a"
alias l="ls -CF"

alias cd..="cd .."

alias g="git"

alias venv="python3 -m venv"

alias vim="$VIM_PROG"
alias e="$EDITOR"
if [[ "$VIM_PROG" == "nvim" ]]; then
    alias vimdiff="nvim -d"
fi

alias k='kubectl'
alias logs='kubectl logs'

alias zyp='sudo zypper'

alias tf='terraform'


if [[ "$TERM" == "xterm-24bit" ]]; then
    alias ssh='TERM=xterm-256color ssh'
fi
