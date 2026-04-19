# shellcheck shell=bash
##########
# PROMPT #
##########

# Dracula palette (256-color approximations)
# Foreground f8f8f2 → 231   Normal/default text
# Cyan       8be9fd → 123   Key labels (vcs=, kube=)
# Orange     ffb86c → 215   CWD path and λ/Δ suffix
# Purple     bd93f9 → 141   Host
# Red        ff5555 → 203   Error status, production kube
TEXT_COLOR="$(tput setaf 231)"
KEY_COLOR="$(tput setaf 123)"
CWD_COLOR="$(tput setaf 215)"
SUFFIX_COLOR="$(tput setaf 215)"
HOST_COLOR="$(tput setaf 141)"
RED="$(tput setaf 203)"
RESET="$(tput sgr0)"

function __git_branch_prompt {
	local ref
	ref="$(git symbolic-ref HEAD 2>/dev/null)" || return
	local branch=${ref#refs/heads/}
	branch=${branch%%[[:space:]]}
	echo -n "$branch"
}

function __git_is_dirty {
	# Returns 0 (true) when the worktree is CLEAN, 1 when dirty.
	# Matches the fish version's semantics.
	local diff
	diff=$(git diff --shortstat 2>/dev/null | tail -n1)
	if [[ -n "$diff" ]]; then
		return 1 # dirty
	else
		return 0 # clean
	fi
}

function __prompt_command {
	local last_status=$?
	_bash_history_sync

	# --- Line 1: path  vcs=<branch>  kube=<context> ---
	PS1="\[${CWD_COLOR}\]\w "

	local branch
	branch=$(__git_branch_prompt)
	if [[ -n "$branch" ]]; then
		PS1+="\[${KEY_COLOR}\]vcs=\[${RESET}\]\[${TEXT_COLOR}\]${branch} "
	fi

	if command -v kubectl >/dev/null 2>&1; then
		local active_context
		active_context=$(kubectl config current-context 2>/dev/null)
		if [[ -n "$active_context" ]]; then
			local kube_val
			if [[ "$active_context" == "production" ]]; then
				kube_val="\[${RED}\]${active_context}"
			else
				kube_val="\[${TEXT_COLOR}\]${active_context}"
			fi
			PS1+="\[${KEY_COLOR}\]kube=\[${RESET}\]${kube_val} "
		fi
	fi

	PS1+="\[${RESET}\]\n"

	# --- Line 2: [pipestatus] @host λ/Δ ---
	# Pipestatus: show non-zero exit code
	if [[ "$last_status" != "0" ]]; then
		PS1+="\[$(tput bold)${RED}\][${last_status}] \[${RESET}\]"
	fi

	# Host
	local host
	host=$(hostname | sed 's/\.local//')
	if [[ -n "$SSH_CLIENT" ]]; then
		host="${USER}@${host}"
	else
		host="@${host}"
	fi
	PS1+="\[${HOST_COLOR}\]${host} \[${RESET}\]"

	# Suffix: λ when clean, Δ when dirty (matches fish prompt)
	if __git_is_dirty; then
		PS1+="\[${SUFFIX_COLOR}\]λ "
	else
		PS1+="\[${SUFFIX_COLOR}\]Δ "
	fi

	PS1+="\[${RESET}\]"
}

PROMPT_COMMAND=__prompt_command
