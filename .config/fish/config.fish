if status is-interactive
	if command -q mise
		mise activate fish | source
	end
end

# OpenClaw Completion
if test -f "/home/chasinglogic/.openclaw/completions/openclaw.fish"
    source "/home/chasinglogic/.openclaw/completions/openclaw.fish"
end

# gib
set --export GIB_INSTALL /Users/chasinglogic/.gib
set --export PATH /Users/chasinglogic/.gib/bin $PATH
