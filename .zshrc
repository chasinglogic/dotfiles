export PATH="/usr/local/opt/helm@2/bin:$PATH"

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git
    docker
    dotenv
    helm
)

echo "Sourcing oh-my-zsh..."
source $ZSH/oh-my-zsh.sh

# User configuration

echo "Sourcing profile..."
source $HOME/.profile
echo "Sourcing prompt..."
source $HOME/.prompt.zsh
echo "Sourcing functions..."
source $HOME/.functions.sh
echo "Sourcing .env.sh..."
source_if_exists $HOME/.env.sh
echo "Sourcing aliases..."
source_if_exists $HOME/.aliases.sh
echo "Sourcing local environment..."
source_if_exists $HOME/.local.sh

export PYENV_ROOT="$HOME/.pyenv"
if [[ -d "$PYENV_ROOT" ]]; then
    command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
fi

# Some Oh-my-zsh thing aliases this to a git command
unalias gam
function gam() { "/Users/chasinglogic/bin/gam/gam" "$@" ; }

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
