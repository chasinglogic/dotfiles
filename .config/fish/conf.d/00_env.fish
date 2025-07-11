set -gx SHELL (which fish)

# Just show me the output please....
set -gx AWS_PAGER
# Google should just make this default since it is required...
set -gx USE_GKE_GCLOUD_AUTH_PLUGIN True
# Plasma scale with HIDPI
set -gx PLASMA_USE_QT_SCALING 1
# Packer's colorized output messes with terminals and other programs I use.
set -gx PACKER_NO_COLOR 1
# GPG can weirdly hang without this I've found
set -gx GPG_TTY (tty)
# Needed for the go compiler and tooling
set -gx GOPATH "$HOME/Code/go"
# Make helm work with our internal chart museum
set -gx GODEBUG x509ignoreCN=0

# Make ripgrep use my config file.
set -gx RIPGREP_CONFIG_PATH "$HOME/.ripgreprc"
# Make XDG_CONFIG_HOME the same on all platforms.
set -gx XDG_CONFIG_HOME "$HOME/.config"

# Mac specific fixes
if test (uname) = Darwin
    set -gx OBJC_DISABLE_INITIALIZE_FORK_SAFETY YES
    set -gx CLICOLOR 1
    set -gx GRPC_PYTHON_BUILD_SYSTEM_OPENSSL 1
    set -gx GRPC_PYTHON_BUILD_SYSTEM_ZLIB 1
    set -gx LDFLAGS "-L/opt/homebrew/opt/openssl@3/lib"
    set -gx CPPFLAGS "-I/opt/homebrew/opt/openssl@3/include"
end

set -gx COLORTERM truecolor

# Storage for miscellaneous or system specific environment variables
set -l local_env_vars "$HOME/.env.fish"
if test -f $local_env_vars
    source $local_env_vars
end

fish_add_path --path /opt/homebrew/bin
fish_add_path --path "$HOME/.local/bin"
fish_add_path --path "$HOME/.elixir-ls/dist"
fish_add_path --path "$GOPATH/bin"
fish_add_path --path "$HOME/.cargo/bin"
fish_add_path --path "/Applications/PyCharm CE.app/Contents/MacOS"
fish_add_path --path "/Applications/PyCharm.app/Contents/MacOS"
fish_add_path --path "/Applications/Docker.app/Contents/Resources/bin"
fish_add_path --path "$HOME/.pulumi/bin"
fish_add_path --path "$HOME/.krew/bin"
fish_add_path --path "$HOME/.krew/bin"
fish_add_path --path "/opt/google-cloud-sdk/bin"

# These have to be after the $PATH is set up.

# FZF default find command
if command -q fd
    set -gx FZF_DEFAULT_COMMAND "fd --type f --hidden --exclude '.git/'"
else
    set -gx FZF_DEFAULT_COMMAND "find . -path './.git' -prune -o -type f -print"
end

if command -q nvim
    set -gx VIM_PROG nvim
else
    set -gx VIM_PROG vim
end

if test -z "$EDITOR"
    set -gx EDITOR "$VIM_PROG"
end
