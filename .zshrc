source "$HOME/.profile"
if command -v mise >/dev/null 2>&1; then
	eval "$(mise activate zsh)"
fi
if command -v starship >/dev/null 2>&1; then
	eval "$(starship init zsh)"
else
	source_if_exists "$HOME/.prompt.zsh"
fi
source_if_exists "$HOME/.functions.sh"
source_if_exists "$HOME/.env.sh"
source_if_exists "$HOME/.aliases.sh"
source_if_exists "$HOME/.local.sh"

HISTFILE="$HOME/.zsh_history"
HISTSIZE=1000000
SAVEHIST=1000000
setopt EXTENDED_HISTORY SHARE_HISTORY HIST_IGNORE_DUPS HIST_IGNORE_SPACE

# Use Emacs line editing even when EDITOR is nvim.
bindkey -e

autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line
bindkey '^[[A' history-beginning-search-backward
bindkey '^[[B' history-beginning-search-forward

if [[ -d "$HOME/.docker/completions" ]]; then
	fpath=("$HOME/.docker/completions" $fpath)
fi

if [[ -d "$HOME/.local/share/zsh/site-functions" ]]; then
	fpath=("$HOME/.local/share/zsh/site-functions" $fpath)
fi

autoload -Uz compinit
compinit

zmodload zsh/complist
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

if command -v terragrunt >/dev/null 2>&1; then
	autoload -U +X bashcompinit && bashcompinit
	complete -o nospace -C "$(command -v terragrunt)" terragrunt
fi

# Load fzf's native key bindings and fuzzy completion after compinit.
if command -v fzf >/dev/null 2>&1; then
	export FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS:+$FZF_DEFAULT_OPTS }--bind=ctrl-k:kill-line"
	# Prefer stronger, shorter matches over recency in history search.
	export FZF_CTRL_R_OPTS="--scheme=default --tiebreak=length"
	source <(fzf --zsh)
fi
