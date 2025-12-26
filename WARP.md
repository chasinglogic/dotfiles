# WARP.md

This file provides guidance to WARP (warp.dev) when working with code in this repository.

## Repository purpose

This repository is a dfm (dotfiles manager) profile for the user `chasinglogic`. It represents a `$HOME`-style tree containing shell, editor, terminal, window manager, and tool configuration files. The `.dfm.yml` file controls how pieces of this tree are mapped into the real home directory and declares an external `secrets` module; the actual secrets live in a separate repository and are not stored here.

## Layout and architecture

### Shell environment and startup

- `.profile` is the central cross-shell configuration. It defines helper functions like `debug`, `find_executable`, `source_if_exists`, and `add_to_path`, sets core environment variables (AWS, GOPATH, RIPGREP_CONFIG_PATH, XDG_CONFIG_HOME, etc.), manages OS-specific tweaks (especially for macOS and Postgres), and establishes the default `$EDITOR`/`$VIM_PROG` and PATH layout. Optional and machine-local settings are sourced via `source_if_exists` from files like `.env.bash`, `.env.local`, `.aliases.local.sh`, and various tool-specific env scripts.
- `.bashrc` configures interactive Bash: history behaviour (`HISTCONTROL`, `HISTSIZE`, `HISTFILESIZE`, `histappend`), globbing options (`globstar`, `extglob`, `direxpand`, `dirspell`, `autocd`), and then sources `.profile` and `.functions.sh`. It also loads completion scripts from various system and Homebrew locations, plus any custom completions in `.local/share/bash-completions`, and sets up completion for tools like `terragrunt`.
- `.zshrc` configures Zsh using `oh-my-zsh` with a set of plugins (git, docker, kubectl, language tools, etc.). After loading Oh My Zsh it sources `.profile`, `.functions.sh`, `.env.sh`, `.aliases.sh`, and `.local.sh` (if present), then sets up completion via `compinit` and `bashcompinit`, including completions for `terragrunt`.
- `.env.sh` is a small helper that sets `SHELLNAME` based on which shell is running.
- `.aliases.sh` centralizes aliases for common tools: `g` → `git`, `k` → `kubectl`, `tf` → `tofu`, `tg`/`grunt` → `terragrunt`, `pm` → `podman`, `dk` → `docker`, `dce` → `docker compose exec -it`, `venv` → `python3 -m venv`, `mypy` → `dmypy check`, distro package manager shortcuts, and AWS/CodeArtifact/ECR login helpers. On Darwin, it aliases `sed` and `grep` to the GNU versions (`gsed`, `ggrep`).
- `.functions.sh` contains higher-level shell helpers:
  - `start_incident`: runs `wezterm record` to start recording the terminal session.
  - `dotfiles`: `cd` into the dfm-managed dotfiles directory (`dfm where`), effectively jumping to the root of this profile.
  - `sp`: project switcher that uses `projector` and `fzf` to find a project (by query or interactive list), `cd`s into it, and sources `.env.local` if present.
  - `v`: Python virtualenv helper that deactivates any existing venv, detects the project top-level via `git rev-parse --show-toplevel` (or `pwd` if not in a repo), chooses an env directory (`env/`, `venv/`, or `.venv/`), creates a venv via `python3 -m venv --prompt <project-name>` if needed (and installs `wheel`), then activates it.
  - `kctx`: wrapper around `kubectl config` to list or switch contexts.
  - `bookmark`: writes `c.<name>` aliases into `.aliases.local.sh` pointing to the current directory, then sources that file.
  - `color` / `color_table`: helpers to print terminal color palettes.

Together, these files make `.profile` the root of shared logic, with `.bashrc` and `.zshrc` acting as thin, shell-specific entrypoints that load shared helpers and then add per-shell behaviour.

### Editor and IDE configuration

- Neovim is configured via `.config/nvim/init.lua` using modern Lua APIs and Neovim's built-in `vim.pack.add` plugin manager.
  - The top of the file sets core options: folding behaviour, search (`ignorecase`/`smartcase`), grep program (preferring `rg`), `wildignore`, undo files, breakindent, autoindent/smartindent, `incsearch`/`inccommand`, UI toggles (line numbers, wrap, signcolumn, clipboard), tab width (4 spaces with `expandtab`), completion behaviour (`completeopt`), diagnostics, and the `<Space>` leader and local leader.
  - `vim.pack.add` defines the plugin stack: Catppuccin theme, `nvim-lspconfig`, Mason and Mason-LSP integration, `mason-tool-installer`, the `mini.nvim` suite, `nvim-treesitter`, `conform.nvim` for formatting, `oil.nvim` for file navigation, `dropbar.nvim`, `vim-fugitive`, `vim-endwise`, `vim-eunuch`, `conjure`, and others.
  - Treesitter is configured via a `treesitter_langs` list that enumerates supported languages (bash, clojure, go, python, rust, terraform, typescript, etc.), installs them via `require('nvim-treesitter').install`, and starts treesitter with proper folding and indent options on the corresponding `FileType` events.
  - The `mini.*` modules are initialized for pairs, surround, split/join, comments, icons, completion, notifications, statusline, pickers, alignment, indent guides, keymaps, movement, highlighters, and extra utilities. Keymaps for completion and movement integrate with `mini.completion` and `mini.pairs`.
  - Mason and `mason-tool-installer` are configured to ensure a wide range of LSP servers, linters, and formatters are installed (e.g. `pyright`, `ruff`, `rust-analyzer`, `gopls`, `eslint_d`, `shellcheck`, `shfmt`, `terraform-ls`, `tflint`, `typescript-language-server`, and others).
  - `conform.nvim` is used for formatting (currently configured explicitly for `eruby` and integrated via `formatexpr` and a `:Format` user command that supports ranges).
  - Keymaps define core behaviours: `fd` escape in insert/terminal modes, Mac-specific `<M-3>` → `#`, window/tab navigation, save/quit, invoking Oil (`-`), shell command execution, git status (`<leader>gs`), and various `Pick` commands for files, grep, buffers, and commands.
  - An `LspAttach` autocmd sets up LSP buffer-local keymaps (rename, code actions, goto definition, hover/documentation, signatures, declaration) and logs that LSP integration is enabled. A `DiagnosticChanged` autocmd keeps the location list populated from diagnostics.
  - Additional autocmds manage quickfix/loclist windows, yank highlighting, and friendlier terminal buffers on `TermOpen`. The colorscheme is set to `catppuccin-mocha` at the end.

### Terminal and multiplexer configuration

- `.wezterm.lua` configures WezTerm using a `config_builder` instance and a small `is_os` helper based on `wezterm.target_triple`.
  - Appearance: Catppuccin Macchiato color scheme, OS-dependent font size (larger on macOS), custom window frame font, window padding, and window decorations (`NONE` on Linux, `RESIZE` elsewhere). The tab bar is hidden when only one tab exists, and scrollbars are disabled.
  - Keybindings: F11 toggles fullscreen; on Linux, `ALT+1`–`ALT+9` activate tabs 1–9. Mouse bindings allow `CTRL+LeftClick` to open links.
  - Startup: a `gui-startup` handler maximizes the initial window. `fish` is detected via `wezterm.glob` (`/usr/bin/fish` on Linux, `/opt/homebrew/bin/fish` on macOS) and, if found, set as `default_prog` with a login shell (`-l`). Missing-glyph warnings are disabled.
- `.tmux.conf` customizes tmux:
  - Changes the prefix key to `M-l` (Alt-l) with a secondary prefix `C-l`.
  - Uses vi-like pane/window navigation and splitting, and keybindings for session selection and pane killing.
  - Enables mouse support, focus events, a large scrollback history, and a more generous display time for tmux messages.
  - Upgrades `$TERM` to `xterm-256color` and enables truecolor (`terminal-features`/`terminal-overrides`).
  - Applies a Catppuccin Mocha-derived theme via a set of color variables and styles for status line, panes, menus, and clock.

### Other tool configuration

- `.config` contains configuration directories for tools including Neovim (`nvim`), Fish (`fish`), terminal emulators (alacritty, kitty, foot, ghostty), Wayland/sway/hyprland/waybar/mako/fuzzel/niri, `direnv`, `mise`, `nushell`, `rofi`, and `systemd` user units. Most of these are standard per-tool configs driven by the tools themselves.
- `.flake8` defines Python linting defaults (max line length 88, ignoring `E203`, `W503`, and `Q000`).
- `.ripgreprc` configures ripgrep to include hidden files, skip `.git/`, and use smart-case searches.
- `.clj-kondo/` holds configuration for the Clojure linter, aligning with Clojure-related settings in Neovim.
- Git configuration is split among `.gitconfig`, `.gitignore`, `.gitignore_global`, `.githooks/`, and `.work.gitconfig`, reflecting both global and work-specific Git settings.

### dfm configuration and modules

- `.dfm.yml` controls how this profile is applied by dfm:
  - The `mappings` section describes how certain paths are treated, including linking `.config/nvim/snippets` and `playbooks/workstation` as directories, and applying OS-specific mappings like the Darwin-only `gpg-agent.conf` destination.
  - The `modules` section references a `secrets` repository (`git@github.com:chasinglogic/secrets.git`), indicating that sensitive content is kept out of this repo and mounted by dfm when present.

## Commands and usage patterns

This repository does not define a traditional build, lint, or test workflow for itself; instead, it configures the development environment used in other projects. The most relevant commands and helpers for day-to-day development are:

- **Dotfiles and environment management**
  - `dotfiles`: quickly jump to the dfm-managed dotfiles repository root (`dfm where`). Use this when you want to edit the profile itself.
  - `start_incident`: begin recording the current WezTerm session (`wezterm record`), useful for capturing debugging sessions or incidents.

- **Project navigation and environments**
  - `sp [query]`: jump to a project directory using `projector` and `fzf`. Without arguments it shows an interactive list; with a query it uses a regex search via `projector find`. After changing directories it sources `.env.local` if present, which is often where per-project secrets or environment overrides live.
  - `v`: standard way to manage Python virtual environments in projects:
    - Determines the project root via `git rev-parse --show-toplevel` or falls back to the current directory.
    - Chooses an environment directory in order: `env/`, `venv/`, `.venv/`.
    - If the directory does not exist, runs `python3 -m venv --prompt <project-name> <dir>`, activates it, and installs `wheel`.
    - On subsequent calls, simply activates the existing env.

- **Shell helpers**
  - `kctx [context]`: with an argument, switches the active Kubernetes context; without arguments, lists available contexts.
  - `bookmark [name]`: creates a persistent alias `c.<name>` that `cd`s into the current directory, stored in `.aliases.local.sh`.
  - Common aliases that affect how commands behave:
    - `g` → `git`
    - `venv` → `python3 -m venv`
    - `k` / `logs` → `kubectl` / `kubectl logs`
    - `tf`, `tg`, `grunt` → `tofu` / `terragrunt`
    - `pm`, `dk`, `dce` → `podman`, `docker`, `docker compose exec -it`
    - `mypy` → `dmypy check`
    - Various `zyp`, `pac`, etc. aliases for distro-specific package managers
    - On macOS, `sed` and `grep` point to GNU implementations (`gsed`, `ggrep`).

- **Linting and search defaults (used across projects)**
  - Python style and linting are expected to conform to `.flake8` (88-char lines, with E203/W503/Q000 ignored).
  - Ripgrep invocations (both from the shell and via Neovim) honour `.ripgreprc`, meaning hidden files are searched (except `.git`) and case-sensitivity is smart.

When working inside this repo itself, your main activities are editing configuration files and keeping behaviour consistent across shells and operating systems. There are no repository-local test suites or build artefacts; validation is typically performed by launching the relevant tool (shell, terminal emulator, Neovim, window manager) and confirming behaviour.

## Guidelines for Warp when editing

- Treat `.profile` as the authoritative location for shared environment configuration and helper functions. If you need to add a cross-shell environment variable or PATH entry, prefer adding it here using `add_to_path` and `source_if_exists` patterns.
- Keep `.bashrc` and `.zshrc` thin and focused on shell-specific concerns (prompt, plugins, shell options), continuing to delegate shared logic to `.profile` and `.functions.sh`.
- Use `source_if_exists` rather than unconditional `source` when referencing files that may not exist on all machines (e.g. local env or tool-specific scripts), to preserve portability.
- For Neovim:
  - Add new plugins to the existing `vim.pack.add` block in `.config/nvim/init.lua`, grouping them logically with related plugins.
  - Expand Treesitter language support by editing the `treesitter_langs` list and relying on the existing autocmds, rather than configuring Treesitter ad hoc elsewhere.
  - Register additional LSPs, linters, or formatters via Mason and `mason-tool-installer` so they are consistently installed, and extend `conform` for formatting where appropriate.
  - Prefer using the established `<leader>` keymap conventions (`<leader>f*` for file operations, `<leader>w` for windows, `<leader>t*` for tabs, `<leader>g*` for git, `<leader>p*` for pickers) rather than inventing unrelated mappings.
- For terminal and tmux configs, follow the existing patterns:
  - In `.wezterm.lua`, modify the `config` table and respect OS checks performed via `is_os`/`is_linux`.
  - In `.tmux.conf`, be aware that the prefix is `M-l` and the theme is Catppuccin-based; avoid conflicting keybindings or theme changes that break the existing palette.
- When adding OS-specific behaviour anywhere in the repo, mirror existing patterns: use `uname` checks or `target_os` fields, guard filesystem assumptions with `if [[ -d ... ]]` or equivalent, and keep macOS-specific logic separated from Linux-specific logic as already done in `.profile`, `.dfm.yml`, and `.wezterm.lua`.
