# AGENTS.md

This guide distills the WARP.md guidance (now removed) into a single source for agentic contributors touching this dfm profile. It focuses on how to validate edits and how to stay within the established coding and configuration conventions.

## Repository Context
- Treat this tree as a `$HOME` snapshot managed by dfm (`.dfm.yml` defines mappings and the external `secrets` module). Do not assume a conventional app layout.
- `.profile` is the canonical place for shared environment variables, helper functions (`debug`, `find_executable`, `source_if_exists`, `add_to_path`), and cross-shell logic.
- `.bashrc` and `.zshrc` are intentionally thin: they set shell-local options, source `.profile`, then bring in `.functions.sh`, `.aliases.sh`, and optional machine-local overlays via `source_if_exists`.
- `.aliases.sh` centralizes ergonomic command shortcuts (`g`, `venv`, `k`, `tf`, etc.) while `.functions.sh` hosts higher-level workflows (`dotfiles`, `sp`, `v`, `kctx`, `bookmark`). Extend them only when logic is broadly reusable across projects.
- `.config/` holds downstream tool configs (Neovim, tmux, WezTerm, distros, wayland compositors). Each tool expects its native file layout, so respect per-tool conventions.
- `.wezterm.lua` and `.tmux.conf` implement Catppuccin-themed terminal environments with OS-aware behaviour. `.profile` already injects OS-specific flags, so avoid duplicating those in other files.
- Secrets are **not** stored here; `.dfm.yml` mounts `git@github.com:chasinglogic/secrets.git`. Never attempt to add secrets to this repo.

## Workflow Overview
- Jump to the repo root via the `dotfiles` shell function (`dfm where`). This avoids confusion when PATH or env state depends on being inside the managed tree.
- When editing per-machine overrides, use `source_if_exists` hooks (`.env.local`, `.aliases.local.sh`, etc.) rather than committing machine-specific changes.
- OS-conditional behaviour follows the patterns already in `.profile` and `.wezterm.lua` (e.g., `if [[ "$(uname)" == "Darwin" ]]`). Match those guards when adding new conditions.
- For Python work in downstream projects, prefer the `v` helper to manage `.venv` directories. It finds or bootstraps envs and installs `wheel` automatically.
- `sp` (projector-based switcher) is the canonical way to hop between projects; if you script project navigation, build on that function.

## Key Files & Directories
- `.profile`: shared env configuration, helper definitions, cross-shell PATH setup, OS branches.
- `.bashrc` / `.zshrc`: thin shell entry points; configure history/options, then source `.profile`, `.functions.sh`, `.aliases.sh`.
- `.functions.sh`: reusable workflows (project switching, venv activation, kube context helpers, color tables).
- `.aliases.sh`: command shortcuts, package-manager helpers, AWS/ECR login flows, GNU replacements on macOS.
- `.config/nvim`: Lua-based Neovim configuration using `vim.pack`, Mason, Treesitter, and Catppuccin theme.
- `.config/wezterm.lua` and `.tmux.conf`: terminal + multiplexer styling and keymaps tuned for Catppuccin.
- `.dfm.yml`: authoritative mapping between this tree and the real home directory; keep structure synchronized here.
- `playbooks/`: Ansible-style automation for workstations; follow YAML conventions when editing.
- `.githooks/`: repo-scoped git hooks; inspect before changing workflow-critical commands.
- `.ripgreprc`, `.flake8`, `.gdu.yaml`: tool-specific defaults that downstream scripts rely upon.
- `.gemini/`, `.local/`, `.ssh/`: per-tool configs; avoid touching machine-unique data unless intended.

## Shell Startup Flow
- Login shells call `.profile` first; interactive shells then source `.bashrc` or `.zshrc` which in turn re-source `.profile` for idempotency.
- `.profile` loads optional layers in this order: `.env.bash`, global tool env scripts, `add_to_path` entries, language-specific env files (`.cargo/env`, `.ghcup/env`), nix hooks, `.env.local`, `.aliases.sh`, `.aliases.local.sh`.
- Zsh also sources `.env.sh` and `.prompt.zsh`; Bash uses `.prompt.bash`.
- Always add new optional scripts to the relevant `source_if_exists` chain instead of forcing them globally.
- Keep exported env vars near related logic (e.g., AWS settings with other cloud vars) to maintain readability.

## Editing Patterns & Tips
- Prefer additive changes; do not remove aliases or helper hooks without confirming no other file references them.
- When touching OS-conditional blocks, update both macOS and Linux paths if applicable to keep parity.
- If adding commands that require credentials, make them configurable via env vars sourced from `.env.local` instead of hardcoding values.
- Use `debug` for verbose output while developing shell helpers; respect `CL_DEBUG` levels.
- Keep long-running commands (like `fzf` pipelines) in `.functions.sh` rather than scattering them across multiple files.
- When reorganizing directories, update `.dfm.yml` mappings plus any existing symlinks to avoid dangling references.

## Build / Lint / Test Commands
| Concern | Command | Notes |
| --- | --- | --- |
| dfm dry-run | `dfm apply --dry-run` | Shows what would be linked without touching the live home directory; safe preflight after structural edits. |
| dfm sync | `dfm apply` | Applies mappings defined in `.dfm.yml`; run after adding or moving tracked files. |
| dfm status | `dfm status` | Displays drift between repo and installed dotfiles. Useful before/after large changes. |
| Shell lint | `shellcheck path/to/script` | Repo shell files already declare `# shellcheck shell=bash`; run per file when editing `.profile`, `.functions.sh`, etc. |
| Lua check | `nvim --headless "+lua vim.cmd('checkhealth')" +qa` | Validates the Neovim config loads under `vim.pack`. Also use `nvim --headless "+luafile %" +qa` for a specific Lua file. |
| Python lint | `flake8 path/to/file.py` | `.flake8` sets max line length 88 and ignores `E203,W503,Q000`; even though Python is sparse here, use this baseline. |
| Python type | `dmypy check module_or_file` | Exposed via the `mypy` alias. Ideal for single-module verification if you add typed helpers. |
| YAML check | `yamllint file.yaml` | Keeps `.dfm.yml` and playbooks valid; prefer `yamllint -d relaxed` if schema noise appears. |
| Editor config | `nvim --headless "+PackerSync" +qa` | With `vim.pack.add`, use `nvim --headless "+lua vim.cmd('Lazy sync')"` if migrating plugin managers; ensures plugin manifest coherence. |

## Targeted Validation (Single-Test Analogs)
- Shell: `shellcheck .profile` or `shellcheck -x path/to/script` for include-aware linting; treat this as the "single test" per touched shell file.
- Lua: `nvim --headless "+luafile path/to/file.lua" +qa` executes just the edited module and fails fast on syntax/runtime issues.
- Python: `flake8 path/to/module.py` or `dmypy check path/to/module.py` keeps new helper scripts consistent without scanning the entire repo.
- dfm: `dfm diff --stat` previews the effect of `.dfm.yml` tweaks without performing a full apply.
- tmux: `tmux source-file ~/.tmux.conf` reloads only the tmux config you touched; `tmux show -g | grep option` confirms values.
- WezTerm: `wezterm ls` verifies the config parsed; to hot-reload, run `wezterm cli reload-config`.

## Validation Examples
- Editing `.profile`: run `shellcheck .profile`, launch a fresh login shell (`bash -l`) to ensure no regressions, and verify PATH changes via `command -v <tool>`.
- Tweaking `.config/nvim/init.lua`: execute `nvim --headless "+luafile .config/nvim/init.lua" +qa` and open Neovim interactively once to inspect UI/theme impacts.
- Adjusting `.dfm.yml`: `yamllint .dfm.yml`, then `dfm diff --stat` to confirm only intended mappings change.
- Updating `.wezterm.lua`: `wezterm cli reload-config` will report parse errors; also run `wezterm ls` to ensure sessions enumerate correctly.
- Changing `.tmux.conf`: `tmux source-file ~/.tmux.conf` and monitor `tmux display-message` for any warnings.
- Editing shell hooks under `.githooks/`: `bash -n path/to/hook` and run the hook manually with representative env vars before relying on git.

## Code Style – General
- **Formatting**: Default to spaces, ASCII, and trailing newline. Python follows the `.flake8` 88-character rule; shell and Lua should stay readable within ~100 chars when possible.
- **Imports / requires**: Group standard libs before third-party modules. In Lua (`init.lua`), require built-ins (`vim`, `vim.opt`) first, then plugin modules, then local modules.
- **Types**: Python snippets should include type hints when practical to keep `dmypy` useful. Lua relies on descriptive variable names; document contract via short comments when the code’s intent is not obvious.
- **Naming**: Shell functions and variables are lower_snake (`start_incident`), global env vars remain ALL_CAPS. Lua locals are lowerCamel or snake consistent with existing Neovim config.
- **Error handling**: Favor explicit exit-code checks (`if [[ $? != 0 ]]`) as seen in `sp` and `v`. For Lua, guard plugin-dependent code with `pcall(require, 'module')` before using it.
- **OS checks**: Keep OS-conditional logic centralized. `.profile` already exports macOS-specific flags; new OS gates should live near existing ones to avoid drift.
- **Secrets**: Never commit values that belong in the external secrets repo; add new placeholders plus a mention in `.dfm.yml` if necessary.

## Code Style – Shell Files
- Keep `# shellcheck shell=bash` headers so CI-like lint commands know the dialect.
- Place helper definitions (`debug`, `source_if_exists`) before they are used, mirroring `.profile`.
- Reuse `source_if_exists` whenever referencing optional files; do not `source` unconditionally unless the file is guaranteed to exist.
- Use `[[ ... ]]` for tests, quote variables, and prefer arrays or `--` to handle spaces (e.g., `kubectl config use-context "$@"`).
- When extending `PATH`, go through `add_to_path` so duplicates stay controlled and debug logging stays consistent.
- Align alias additions with the style inside `.aliases.sh`: simple `alias name="command"` lines grouped by tool; keep OS-specific aliases within existing `if [[ "$(uname)" == "Darwin" ]]` guards.
- For user-facing helpers (e.g., `sp`), echo informative errors and return the upstream exit code so scripts can build upon them.

## Code Style – Neovim Lua
- Plugins are declared via `vim.pack.add` at the top of `.config/nvim/init.lua`. When adding a plugin, keep alphabetical-ish grouping with related tooling (LSP, UI, git, etc.).
- Extend Treesitter support by editing the `treesitter_langs` list; the existing autocmd installs parsers for each listed language, so avoid ad hoc installs elsewhere.
- LSP servers, linters, and formatters are sourced through Mason and `mason-tool-installer`. Append to the `ensure_installed` arrays to guarantee consistent bootstrap behaviour.
- Formatting is delegated to `conform.nvim`; register new formatters there instead of calling external binaries manually.
- Reuse the keymap conventions already defined: `<leader>g*` for git, `<leader>f*` for files, `<leader>p*` for Telescope-style pickers, `<leader>w*` for windows.
- For diagnostics, rely on the `LspAttach` autocmd infrastructure. New buffer-local mappings should be added inside that autocmd rather than scattered elsewhere.
- When editing colors or UI, stay within the Catppuccin theme choices to keep tmux/wezterm/neovim visually aligned.

## Neovim Workflow Tips
- Mason installs run automatically on startup; if you alter `ensure_installed`, trigger `:MasonToolsInstall` or run `nvim --headless "+MasonToolsInstall" +qa` once.
- Treesitter languages are installed lazily via autocmd. To force install, run `:TSInstallSync <lang>` after updating the table.
- The config sets `<Space>` as both leader and localleader; stick to this mapping scheme for new shortcuts.
- Oil (`-`) opens the floating file browser; respect that its behaviour is bound globally and avoid overriding the key.
- Use `MiniPick` for new pickers; existing `Pick files`, `Pick grep`, and `Pick buffers` wrappers illustrate the pattern.
- Autocmds for diagnostics populate the location list. If you add new diagnostic consumers, update the existing autocmd block instead of creating duplicates.
- Term buffers have custom settings via `TermOpen`; additional terminal tweaks should live in that autocmd to maintain parity.

## Code Style – Python and Misc Configs
- Python helpers (rare) should run under `python3`. Respect `.flake8` (88 columns, ignore `E203`, `W503`, `Q000`) and keep imports grouped: stdlib, third-party, local.
- When writing YAML (e.g., `.dfm.yml`, playbooks), use two-space indents, key ordering that highlights mappings (`mappings`, then `modules`), and anchors only if necessary.
- TOML/INI files (e.g., `.wezterm.lua` uses Lua but `.projector.*` uses other formats) should maintain existing spacing and comment styles.
- For tmux and WezTerm, keep Catppuccin color constants and OS branching logic intact; add new bindings with descriptive comments if behaviour differs from defaults.
- Ripgrep defaults come from `.ripgreprc`. If you script search commands, rely on `rg` respecting `--hidden`, `--glob=!.git/*`, and `--smart-case`.

## Tooling & Integration Notes
- `dfm`: After structural changes, run `dfm status` followed by `dfm apply --dry-run` before linking to a real $HOME. Document new mappings in `.dfm.yml` with clear `match` patterns.
- `wezterm`: Config uses `config_builder` and an `is_os` helper based on `wezterm.target_triple`. Additional OS-specific tweaks should branch through that helper.
- `tmux`: Prefix is `C-b`. Avoid adding conflicting bindings. To test, `tmux source-file ~/.tmux.conf` inside an existing session.
- `Neovim`: The config expects `vim.pack`. Avoid mixing plugin managers. To refresh dependencies, `nvim --headless "+lua vim.cmd('Lazy! sync')" +qa` if migrating, else follow the existing pattern.
- `projector` integration: The `sp` function relies on `projector list/find`. If you change its behaviour, keep compatibility with both interactive and query-driven modes.
- `kctx` expects the `kubectl` alias `k`; leave that alias intact. When adding kube helpers, prefer functions in `.functions.sh` so both shells share them.
- `start_incident` uses `wezterm record`; keep it simple to ensure it can run on machines where WezTerm is installed.

## Documentation & Comments
- Comments should explain *why* something is configured in a certain way (e.g., macOS-specific exports) rather than restating code.
- For shell files, keep comments short and sentence-style; prefer block comments only when documenting multi-step flows.
- Lua comments (`--`) live directly above the statement they describe; avoid trailing comments on long tables because they hinder diffs.
- YAML comments belong on their own lines to keep `yamllint` happy; document unusual mappings or OS conditionals when necessary.
- Avoid duplicating WARP/AGENTS content inside other files; instead, link back to this document if more policy needs to be referenced.

## Cursor / Copilot Rules
- There are currently **no** `.cursor/rules` or `.cursorrules` files, and no `.github/copilot-instructions.md`. If such rules are added later, summarize them here for downstream agents.

## Operational Checklist for Agents
- Before editing, skim `.profile` plus any target tool config to understand existing patterns; most files already include context-rich comments.
- After editing shell files, run `shellcheck` and at least source them in a subshell (`bash --noprofile --norc -c "source .profile"`).
- After editing Neovim Lua, run the headless command mentioned above and optionally `nvim +checkhealth` interactively.
- After tweaking `.dfm.yml`, run `yamllint` and `dfm diff --stat`. Never run a destructive `dfm apply` on CI runners.
- Commit messages (when requested) should explain _why_ a tweak is needed (e.g., "Update WezTerm font fallback for Linux" rather than "changes").
- Remember this repo shadows a real $HOME; keep paths relative and avoid absolute `/Users/...` unless they match dfm mappings.
- When in doubt, prefer adding configurable hooks (env vars, `source_if_exists`) instead of hardcoding machine-specific paths.

## Next Steps
- After substantial edits, run the relevant validation commands above and capture their output in your notes; the repo has no CI safety net.
- If you discover new conventions or tools, update this AGENTS.md immediately so future agents inherit the same context.
