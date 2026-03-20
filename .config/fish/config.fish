if status is-interactive
	if command -q mise
		mise activate fish | source
	end
end

# OpenClaw Completion
source "/home/chasinglogic/.openclaw/completions/openclaw.fish"
