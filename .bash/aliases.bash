# Quality of Life
alias vgt="vagrant"
alias emt="emacs -nw"
alias e="$EDITOR"

function n() {
    if [ -f Session.vim ]; then
        nv -S Session.vim $@
    else
        nv -c Obsession $@
    fi
}

function et() {
    emacsclient --tty -a 'vi' $@
}

function ec() {
    emacsclient --no-wait $@
}

function dotfiles() {
    cd $(dfm where)
}

if [[ -x /usr/local/bin/nvim ]]; then
    alias vi="nvim"
    alias vim="nvim"
fi

# ls aliases
alias ll="ls -alF"
alias la="ls -a"
alias l="ls -CF"

# cd aliases
alias cd..="cd .."
alias cdc="cd $HOME/Code"
# As a function since dfm is not always available at login
function cdn() {
    cd "$(dfm where)/Notes"
}

# Common package manager stuff
alias apt="sudo apt"
alias zyp="sudo zypper"
alias dnf="sudo dnf"
alias pca="pacaur"
alias pac="sudo pacman"
alias pacman="sudo pacman"

# Programming language stuff
alias p="python"
alias p3="python3"
alias ve="python3 -m venv"
alias venv="python3 -m venv"

# Systemctl is used too much to sudo that shit.
alias systemctl="sudo systemctl"
alias sctl="sudo systemctl"
alias pg="sudo systemctl start postgresql"

# Git
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

# Ctags
alias ct="ctags ."

# Amazon Web Services (aws)
alias ec2="aws ec2"
alias s3="aws s3"
alias tf="terraform"

# FZF
function fcd() {
    cmd=$(__fzf_cd__)
    $cmd
}

function v() {
    if [ -d .venv ]; then
        source .venv/bin/activate
    elif [ -d venv ]; then
        source venv/bin/activate
    else
        ve .venv
        v
    fi
}
