# Nushell Environment Config File
#
# version = "0.97.1"

def is_git_repo [] {
    '.git' | path exists
}



def create_left_prompt [] {
    let dir = match (do --ignore-shell-errors { $env.PWD | path relative-to $nu.home-path }) {
        null => $env.PWD
        '' => '~'
        $relative_pwd => ([~ $relative_pwd] | path join)
    }

    let path_color = (if (is-admin) { ansi red_bold } else { ansi reset })
    let branch = (if (is_git_repo) { 
        git symbolic-ref HEAD | path basename 
    } else { 
        '' 
    })
    let branch_color = (if $branch in ['main', 'master'] {
        ansi red_bold
    } else if $branch == 'develop' {
        ansi xterm_orangebold1
    } else {
        ansi green
    })
    let branch_prompt = (if $branch == "" { "" } else { $" ($branch)" })
    let last_command_status = (if $env.LAST_EXIT_CODE == 0 { "" } else { $"(ansi red_bold)!! " })

    mut contextual_info = ""
    let context_color = (ansi magenta)
    def context_segment [currentContext: string, key: string, value: string, valueColor = (ansi reset)] {
        if ($value == "") { return $currentContext }
        let prefix = if $currentContext == "" {
            ""
        } else {
            $"($currentContext) "
        }
        $"($prefix)($context_color)[($key): ($valueColor)($value)($context_color)](ansi reset)"
    }

    let kube_context = (kubectl config current-context err> /dev/null)
    let kube_color = (if $kube_context in ['production', 'staging'] { 
        ansi red_bold
    } else {
        ansi reset
    })
    $contextual_info = (context_segment $contextual_info kube $kube_context $kube_color)

    if $contextual_info != "" {
        $contextual_info += "\n"
    }

    $"($contextual_info)($last_command_status)($path_color)($dir)($branch_color)($branch_prompt)(ansi reset) "
}

# Use nushell functions to define your right and left prompt
$env.PROMPT_COMMAND = {|| create_left_prompt }
$env.PROMPT_COMMAND_RIGHT = {||}

# The prompt indicators are environmental variables that represent
# the state of the prompt
$env.PROMPT_INDICATOR = {|| 
    if ((is_git_repo) and (git status --porcelain) != "") {
        return $"(ansi xterm_orange1)Δ "
    } else {
       return $"(ansi xterm_orange1)λ " 
    }
}
$env.PROMPT_INDICATOR_VI_INSERT = {|| ": " }
$env.PROMPT_INDICATOR_VI_NORMAL = {|| "> " }
$env.PROMPT_MULTILINE_INDICATOR = {|| "::: " }

# If you want previously entered commands to have a different prompt from the usual one,
# you can uncomment one or more of the following lines.
# This can be useful if you have a 2-line prompt and it's taking up a lot of space
# because every command entered takes up 2 lines instead of 1. You can then uncomment
# the line below so that previously entered commands show with a single `🚀`.
# $env.TRANSIENT_PROMPT_COMMAND = {|| "🚀 " }
# $env.TRANSIENT_PROMPT_INDICATOR = {|| "" }
# $env.TRANSIENT_PROMPT_INDICATOR_VI_INSERT = {|| "" }
# $env.TRANSIENT_PROMPT_INDICATOR_VI_NORMAL = {|| "" }
# $env.TRANSIENT_PROMPT_MULTILINE_INDICATOR = {|| "" }
# $env.TRANSIENT_PROMPT_COMMAND_RIGHT = {|| "" }

# Specifies how environment variables are:
# - converted from a string to a value on Nushell startup (from_string)
# - converted from a value back to a string when running external commands (to_string)
# Note: The conversions happen *after* config.nu is loaded
$env.ENV_CONVERSIONS = {
    "PATH": {
        from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
        to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
    }
    "Path": {
        from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
        to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
    }
}

# Directories to search for scripts when calling source or use
# The default for this is $nu.default-config-dir/scripts
$env.NU_LIB_DIRS = [
    ($nu.default-config-dir | path join 'scripts') # add <nushell-config-dir>/scripts
    ($nu.data-dir | path join 'completions') # default home for nushell completions
]

# Directories to search for plugin binaries when calling register
# The default for this is $nu.default-config-dir/plugins
$env.NU_PLUGIN_DIRS = [
    ($nu.default-config-dir | path join 'plugins') # add <nushell-config-dir>/plugins
]

# To add entries to PATH (on Windows you might use Path), you can use the following pattern:
# $env.PATH = ($env.PATH | split row (char esep) | prepend '/some/path')
# An alternate way to add entries to $env.PATH is to use the custom command `path add`
# which is built into the nushell stdlib:
# use std "path add"
# $env.PATH = ($env.PATH | split row (char esep))
# path add /some/path
# path add ($env.CARGO_HOME | path join "bin")
# path add ($env.HOME | path join ".local" "bin")
# $env.PATH = ($env.PATH | uniq)

# To load from a custom file you can use:
# source ($nu.default-config-dir | path join 'custom.nu')

$env.CARAPACE_BRIDGES = 'bash'
$env.EDITOR = 'nvim'

# Just show me the output please...
$env.AWS_PAGER = ""
# Needed for the go compiler and tooling
$env.GOPATH = $"($env.HOME)/Code/go"

# Set CCACHE directory
if ('/data/ccache' | path exists) {
	$env.CCACHE_DIR = '/data/ccache'
	$env.CCACHE_MAXSIZE = '200G'
} else {
	$env.CCACHE_MAXSIZE = '20G'
}

$env.PATH = ["/usr/local/bin", "/usr/bin", "/bin", "/sbin"]

def --env add_to_path [dir: string] {
    $env.PATH = ($env.PATH | split row (char esep) | prepend $dir)
}

if ((uname | get operating-system) == 'Darwin') {
    $env.OBJC_DISABLE_INITIALIZE_FORK_SAFETY = 'YES'
    $env.CLICOLOR = 1
    $env.GRPC_PYTHON_BUILD_SYSTEM_OPENSSL = 1
    $env.GRPC_PYTHON_BUILD_SYSTEM_ZLIB = 1
    $env.LDFLAGS = "-L/opt/homebrew/opt/openssl@3/lib"
    $env.CPPFLAGS = "-I/opt/homebrew/opt/openssl@3/include"

    if ($"($env.HOME)/Library/Python" | path exists) {
        for dir in (ls -a | where type == dir) {
            add_to_path dir
        }
    }
}

add_to_path '/opt/homebrew/bin'
add_to_path $"($env.HOME)/.local/bin"
add_to_path $"($env.HOME)/.elixir-ls/dist"
add_to_path $"($env.GOPATH)/bin"
add_to_path $"($env.HOME)/.cargo/bin"
add_to_path "/Applications/PyCharm CE.app/Contents/MacOS"
add_to_path "/Applications/PyCharm.app/Contents/MacOS"
add_to_path $"($env.HOME)/.config/emacs/bin"
add_to_path $"($env.HOME)/.pulumi/bin"
add_to_path "/home/linuxbrew/.linuxbrew/bin"

# Make ripgrep use my config file.
$env.RIPGREP_CONFIG_PATH = $"($env.HOME)/.ripgreprc"
# Make XDG_CONFIG_HOME the same on all platforms.
$env.XDG_CONFIG_HOME = $"($env.HOME)/.config"

