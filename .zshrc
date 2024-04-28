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
    aliases
    aws
    docker
    fd
    helm
    gitfast
    golang
    helm
    kubectl
    mix-fast
    pip
    podman
    ripgrep
    virtualenv
    rust
)

echo "Sourcing oh-my-zsh..."
source "$ZSH/oh-my-zsh.sh"

# User configuration

echo "Sourcing my configs..."
source "$HOME/.profile"
source_if_exists "$HOME/.prompt.zsh"
source_if_exists "$HOME/.functions.sh"
source_if_exists "$HOME/.env.sh"
source_if_exists "$HOME/.aliases.sh"
source_if_exists "$HOME/.local.sh"

# initialise completions with ZSH's compinit
autoload -Uz compinit && compinit

if [[ -x $(which atuin 2>/dev/null) ]]; then
    echo "Setting up atuin..."
    eval "$(atuin init zsh --disable-up-arrow)"
fi

