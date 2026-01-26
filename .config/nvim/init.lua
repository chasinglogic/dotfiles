-- vim: foldmethod=marker foldlevel=0

-- Options {{{
-- Enable modelines
vim.o.modeline = true
-- Start with all folds open
vim.o.foldlevelstart = 99
-- fo-n Recognize numbered lists
vim.opt.formatoptions:append({
    "n",
})
-- If the 'ignorecase' option is on, the case of normal letters is ignored.
-- 'smartcase' can be set to ignore case when the pattern contains lowercase
-- letters only. Note that this affects mini.pick as well.
vim.o.ignorecase = true
vim.o.smartcase = true
-- Use ripgrep if it's available
if vim.fn.executable("rg") then
    vim.opt.grepprg = "rg --vimgrep --no-heading --smart-case"
else
    vim.opt.grepprg = "grep -R"
end
-- Ignore these kinds of files when pressing <TAB> to complete a command.
vim.opt.wildignore:append({ "*.o", "*.git", "*.svn", "*.pyc", "env/*", ".git/*" })
-- automatically save your undo history when you write a file and restore undo
-- history when you edit the file again
vim.o.undofile = true
-- Preserve horizontal blocks of text when indenting
vim.o.breakindent = true
-- Copy indent from current line when starting a new line
vim.o.autoindent = true
-- Tries to be smart about indenting when creating a new line based on language
-- constructs like brackets.
vim.o.smartindent = true
-- Hightly search matches as search is typed.
vim.o.incsearch = true
-- Show incsearch in a split
vim.o.inccommand = "split"
-- Don't show mode in command line (status line has info already)
vim.o.showmode = false
-- Enable line numbers
vim.o.number = true
-- Don't visually wrap lines, if it's too long it's too long.
vim.o.wrap = false
-- Always show sign column to avoid flickering when LSP's are thinking.
vim.o.signcolumn = "yes"
-- Use system clipboard
vim.opt.clipboard:append("unnamedplus")
-- Make tabs 4 spaces
vim.o.tabstop = 4
vim.o.shiftwidth = 4
-- Expand tabs to spaces
vim.o.expandtab = true
--  Don't highlight search matches after search is completed
vim.opt.hlsearch = false
-- A comma-separated list of options for Insert mode completion, used options:
-- fuzzy    Enable fuzzy-matching for completion candidates.  This
--          allows for more flexible and intuitive matching, where
--          characters can be skipped and matches can be found even
--          if the exact sequence is not typed.  Note: This option
--          does not affect the collection of candidate list, it only
--          controls how completion candidates are reduced from the
--          list of alternatives.  If you want to use |fuzzy-matching|
--          to gather more alternatives for your candidate list,
--          see 'completefuzzycollect'.
--
-- menuone  Use the popup menu also when there is only one match.
-- 	        Useful when there is additional information about the
-- 	        match, e.g., what file it comes from.
--
-- noselect Same as "noinsert" (Do not insert any text until user selects it),
--          except that no menu item is pre-selected.  If both "noinsert" and
--          "noselect" are present, "noselect" takes precedence.  This is
--          enabled automatically when 'autocomplete' is on, unless "preinsert"
--          is also enabled.
vim.o.completeopt = 'menuone,fuzzy,noselect'
vim.diagnostic.config({
    -- Show signs on top of any other sign, but only for warnings and errors
    signs = { priority = 9999, severity = { min = 'WARN', max = 'ERROR' } },
    virtual_text = true,
})
-- Set <space> as the leader key
-- See `:help mapleader`
-- NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = " "
vim.g.maplocalleader = " "
-- Make it so dd in the quickfix window removes entries
vim.api.nvim_create_autocmd('FileType', {
    pattern = 'qf',
    callback = function()
        vim.keymap.set('n', 'dd', function()
            local qf_list = vim.fn.getqflist()
            local current_line = vim.fn.line('.')
            if qf_list[current_line] then
                table.remove(qf_list, current_line)
                vim.fn.setqflist(qf_list, 'r')
                local new_line = math.min(current_line, #qf_list)
                vim.fn.cursor(new_line, 1)
            end
        end, { buffer = true, silent = true, desc = 'Remove quickfix item under cursor' })
    end
})
-- }}}
-- Plugins {{{
vim.g["conjure#filetypes"] = {
    "clojure",
    "fennel",
    "janet",
    "hy",
    "julia",
    "racket",
    "scheme",
    "lua",
    "lisp",
    "sql",
}

-- Enable filtering the quickfix list this plugin is bundled with vim.
vim.cmd('packadd cfilter')

-- Bootstrap mini.deps {{{
local mini_path = vim.fn.stdpath("data") .. "/site/pack/deps/start/mini.nvim"
if not vim.loop.fs_stat(mini_path) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/nvim-mini/mini.nvim",
        mini_path,
    })
end
vim.opt.rtp:prepend(mini_path)
-- }}}
-- Install plugins {{{
local MiniDeps = require("mini.deps")
MiniDeps.setup()

MiniDeps.add("catppuccin/nvim")
MiniDeps.add("challenger-deep-theme/vim")
-- LSP helpers
MiniDeps.add("neovim/nvim-lspconfig")
MiniDeps.add("mason-org/mason.nvim")
MiniDeps.add("mason-org/mason-lspconfig.nvim")
MiniDeps.add("WhoIsSethDaniel/mason-tool-installer.nvim")
-- One plugin to rule them all (provides tons of basic functionality)
MiniDeps.add("nvim-mini/mini.nvim")
-- Pre-configured treesitter setups with installation
MiniDeps.add("nvim-treesitter/nvim-treesitter")
-- Autoformatting that uses tools and LSP together happily.
MiniDeps.add("stevearc/conform.nvim")
-- Loads of free snippets
MiniDeps.add("rafamadriz/friendly-snippets")
-- Manage files in neovim without suffering
MiniDeps.add("stevearc/oil.nvim")
-- Give contextual location information in a winbar
MiniDeps.add("Bekaboo/dropbar.nvim")
-- Do git stuff with vim, like magit
MiniDeps.add("tpope/vim-fugitive")
-- Automatically MiniDeps.add end or similar constructs
MiniDeps.add("tpope/vim-endwise")
-- Useful commands like Rename, Delete, SudoWrite
MiniDeps.add("tpope/vim-eunuch")
MiniDeps.add("Olical/conjure")
-- Better auto-complete
MiniDeps.add({
    source = 'saghen/blink.cmp',
    checkout = 'v1.8.0',
})
-- LLM in my NVIM
MiniDeps.add({
    source = 'milanglacier/minuet-ai.nvim',
    depends = { 'nvim-lua/plenary.nvim' },
})
-- Autoclose tags in TSX, HTML, etc.
MiniDeps.add({
    source = 'windwp/nvim-ts-autotag'
})
-- }}}
-- blink.cmp {{{
local blink = require('blink.cmp')
blink.setup({
    -- 'default' (recommended) for mappings similar to built-in completions (C-y to accept)
    -- 'super-tab' for mappings similar to vscode (tab to accept)
    -- 'enter' for enter to accept
    -- 'none' for no mappings
    --
    -- All presets have the following mappings:
    -- C-space: Open menu or open docs if already open
    -- C-n/C-p or Up/Down: Select next/previous item
    -- C-e: Hide menu
    -- C-k: Toggle signature help (if signature.enabled = true)
    --
    -- See :h blink-cmp-config-keymap for defining your own keymap
    keymap = { preset = 'super-tab' },

    appearance = {
        -- 'mono' (default) for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
        -- Adjusts spacing to ensure icons are aligned
        nerd_font_variant = 'mono'
    },

    -- (Default) Only show the documentation popup when manually triggered
    completion = { documentation = { auto_show = false } },

    -- Default list of enabled providers defined so that you can extend it
    -- elsewhere in your config, without redefining it, due to `opts_extend`
    sources = {
        default = { 'lsp', 'path', 'buffer' },
    },

    -- (Default) Rust fuzzy matcher for typo resistance and significantly better performance
    -- You may use a lua implementation instead by using `implementation = "lua"` or fallback to the lua implementation,
    -- when the Rust fuzzy matcher is not available, by using `implementation = "prefer_rust"`
    --
    -- See the fuzzy documentation for more information
    fuzzy = { implementation = "prefer_rust_with_warning" }
})
-- }}}
-- minuet-ai {{{
require('minuet').setup({
    provider = 'gemini',
    virtualtext = {
        auto_trigger_ft = { 'python' },
        keymap = {
            -- accept whole completion
            accept = '<A-a>',
            -- accept one line
            accept_line = '<A-A>',
            -- accept n lines (prompts for number)
            -- e.g. "A-z 2 CR" will accept 2 lines
            accept_n_lines = '<A-z>',
            -- Cycle to prev completion item, or manually invoke completion
            prev = '<A-[>',
            -- Cycle to next completion item, or manually invoke completion
            next = '<A-]>',
            dismiss = '<A-e>',
        },
    },
})
-- }}}
-- DropBar {{{
require('dropbar').setup()
-- }}}
-- TreeSitter {{{
local treesitter_langs = {
    "bash",
    "c",
    "clojure",
    "cpp",
    "css",
    "dockerfile",
    "embedded_template",
    "fish",
    "go",
    "gotmpl",
    "hcl",
    "html",
    "htmldjango",
    "ini",
    "javascript",
    "jsdoc",
    "json",
    "json5",
    "jsonnet",
    "jsx",
    "kdl",
    "lua",
    "python",
    "ruby",
    "rust",
    "svelte",
    "terraform",
    "toml",
    "tsx",
    "typescript",
    "vim",
    "vimdoc",
    "vue",
    "yaml",
    "zig",
}
require('nvim-treesitter').install(treesitter_langs)
vim.api.nvim_create_autocmd('FileType', {
    pattern = treesitter_langs,
    callback = function()
        vim.treesitter.start()
        vim.wo[0][0].foldexpr = 'v:lua.vim.treesitter.foldexpr()'
        vim.wo[0][0].foldmethod = 'expr'
        vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
    end,
})
-- }}}
-- Mini {{{
require('mini.pairs').setup()
require('mini.surround').setup()
require('mini.cmdline').setup()
require('mini.bracketed').setup()
require('mini.splitjoin').setup()
require('mini.comment').setup()
-- Disabled because it bugs out often or I can't figure out how to use it right
-- now.
-- local gen_loader = require('mini.snippets').gen_loader
-- require('mini.snippets').setup({
--     snippets = {
--         gen_loader.from_lang(),
--     }
-- })
require('mini.icons').setup()
-- require('mini.completion').setup()
require('mini.notify').setup()
require('mini.statusline').setup()
-- Centered on screen
local centered_picker_window = function()
    local height = math.floor(0.618 * vim.o.lines)
    local width = math.floor(0.618 * vim.o.columns)
    return {
        anchor = 'NW',
        height = height,
        width = width,
        row = math.floor(0.5 * (vim.o.lines - height)),
        col = math.floor(0.5 * (vim.o.columns - width)),
    }
end
require('mini.pick').setup({ window = { config = centered_picker_window } })
-- Align text interactively. Example usage:
-- - `gaip,` - `ga` (align operator) *i*nside *p*aragraph by comma
-- - `gAip` - start interactive alignment on the paragraph. Choose how to
--   split, justify, and merge string parts. Press `<CR>` to make it permanent,
--   press `<Esc>` to go back to initial state.
--
-- See also:
-- - `:h MiniAlign-example` - hands-on list of examples to practice aligning
-- - `:h MiniAlign.gen_step` - list of support step customizations
-- - `:h MiniAlign-algorithm` - how alignment is done on algorithmic level
require('mini.align').setup()
-- Align text interactively. Example usage:
-- - `gaip,` - `ga` (align operator) *i*nside *p*aragraph by comma
-- - `gAip` - start interactive alignment on the paragraph. Choose how to
--   split, justify, and merge string parts. Press `<CR>` to make it permanent,
--   press `<Esc>` to go back to initial state.
--
-- See also:
-- - `:h MiniAlign-example` - hands-on list of examples to practice aligning
-- - `:h MiniAlign.gen_step` - list of support step customizations
-- - `:h MiniAlign-algorithm` - how alignment is done on algorithmic level
require('mini.align').setup()
-- Visualize and work with indent scope. It visualizes indent scope "at cursor"
-- with animated vertical line. Provides relevant motions and textobjects.
-- Example usage:
-- - `cii` - *c*hange *i*nside *i*ndent scope
-- - `Vaiai` - *V*isually select *a*round *i*ndent scope and then again
--   reselect *a*round new *i*indent scope
-- - `[i` / `]i` - navigate to scope's top / bottom
--
-- See also:
-- - `:h MiniIndentscope.gen_animation` - available animation rules
require('mini.indentscope').setup({
    draw = {
        delay = 0,
        animation = require('mini.indentscope').gen_animation.none(),
    }
})
require('mini.keymap').setup()
-- Navigate 'mini.completion' menu with `<Tab>` /  `<S-Tab>`
MiniKeymap.map_multistep('i', '<Tab>', { 'pmenu_next' })
MiniKeymap.map_multistep('i', '<S-Tab>', { 'pmenu_prev' })
-- On `<CR>` try to accept current completion item, fall back to accounting
-- for pairs from 'mini.pairs'
MiniKeymap.map_multistep('i', '<CR>', { 'pmenu_accept', 'minipairs_cr' })
-- On `<BS>` just try to account for pairs from 'mini.pairs'
MiniKeymap.map_multistep('i', '<BS>', { 'minipairs_bs' })
-- Move any selection in any direction. Example usage in Normal mode:
-- - `<M-j>`/`<M-k>` - move current line down / up
-- - `<M-h>`/`<M-l>` - decrease / increase indent of current line
--
-- Example usage in Visual mode:
-- - `<M-h>`/`<M-j>`/`<M-k>`/`<M-l>` - move selection left/down/up/right
require('mini.move').setup()
local hipatterns = require('mini.hipatterns')
hipatterns.setup({
    highlighters = {
        -- Highlight a fixed set of common words. Will be highlighted in any place,
        -- not like "only in comments".
        -- fixme = MiniExtra.gen_highlighter.words({ 'FIXME', 'Fixme', 'fixme' }, 'MiniHipatternsFixme'),
        -- hack = MiniExtra.gen_highlighter.words({ 'HACK', 'Hack', 'hack' }, 'MiniHipatternsHack'),
        -- todo = MiniExtra.gen_highlighter.words({ 'TODO', 'Todo', 'todo' }, 'MiniHipatternsTodo'),
        -- note = MiniExtra.gen_highlighter.words({ 'NOTE', 'Note', 'note' }, 'MiniHipatternsNote'),

        -- Highlight hex color string (#aabbcc) with that color as a background
        hex_color = hipatterns.gen_highlighter.hex_color(),
    },
})
require('mini.extra').setup()
-- }}}
-- Mason {{{
require("mason").setup()
require("mason-lspconfig").setup()
require("mason-tool-installer").setup({
    ensure_installed = {
        "ansible-language-server",
        "cljfmt",
        "clojure-lsp",
        "docker-compose-language-service",
        "dockerfile-language-server",
        "erb-formatter",
        "erb-lint",
        "eslint_d",
        "gopls",
        "helm-ls",
        "html-lsp",
        "lua_ls",
        "python-lsp-server",
        "rubocop",
        "ruby-lsp",
        "ruff",
        "rust-analyzer",
        "shellcheck",
        "shfmt",
        "stimulus-language-server",
        "terraform-ls",
        "tflint",
        "typescript-language-server",
    }
})
-- }}}
-- Autoformatting (conform) {{{
require('conform').setup({
    formatters_by_ft = {
        eruby = { "erb_format" }
    },
    format_on_save = {
        timeout_ms = 500,
        lsp_format = 'first',
    }
})
vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
vim.api.nvim_create_user_command("Format", function(args)
    local range = nil
    if args.count ~= -1 then
        local end_line = vim.api.nvim_buf_get_lines(0, args.line2 - 1, args.line2, true)[1]
        range = {
            start = { args.line1, 0 },
            ["end"] = { args.line2, end_line:len() },
        }
    end
    require("conform").format({ lsp_format = "fallback", range = range })
end, { range = true })
-- }}}
-- Oil {{{
require('oil').setup({
    view_options = {
        show_hidden = true,
    },
})
-- }}}
-- nvim-ts-autotag {{{
require('nvim-ts-autotag').setup({
    opts = {
        enable_close = true,         -- Auto close tags
        enable_rename = true,        -- Auto rename pairs of tags
        enable_close_on_slash = true -- Auto close on trailing </
    },
})
-- }}}
-- }}}
-- Keys {{{

vim.keymap.set('i', 'fd', '<ESC>')
vim.keymap.set('t', 'fd', '<c-\\><c-n>')

--On MacOS I tell my keyboard to use alt as meta as the Lord intended but that
--means I can't type the hash symbol on my UK keyboards. This fixes that.
if vim.fn.has("macunix") then
    vim.keymap.set("i", "<M-3>", "#")
end

local nmap = function(keys, func, desc)
    vim.keymap.set("n", keys, func, { desc = desc })
end

nmap("-", "<CMD>Oil<CR>", "Open parent directory")
nmap("!", ":!", "Run shell command")

nmap('<leader>?', function()
    require("which-key").show({ global = false })
end, "Buffer local keymaps")

nmap('<leader>fs', '<CMD>w<CR>', '[F]ile [S]ave')
nmap('<leader>fq', '<CMD>wq<CR>', '[F]ile save and [Q]uit')
nmap('<leader>ft', '<CMD>Fyler kind=split_left_most<CR>', '[F]ile [T]ree')

nmap('<leader>w', '<C-w>', '[W]indows')
nmap('<leader>r', '<CMD>source ~/.config/nvim/init.lua<CR>')
nmap('<leader>u', '<CMD>DepsUpdate<CR>', 'Update plugins')

nmap('<leader>to', '<CMD>tabnew<CR>', '[T]ab [O]pen')
nmap('<leader>tc', '<CMD>tabclose<CR>', '[T]ab [C]lose')
nmap('<leader>tn', '<CMD>tabnext<CR>', '[T]ab [N]ext')
nmap('<leader>tp', '<CMD>tabprev<CR>', '[T]ab [P]revious')

nmap('<leader>gs', '<CMD>Git<CR>', '[G]it [S]tatus')
nmap('<leader>gg', ':Git ', '[G]it')

nmap('<leader>pf', '<CMD>Pick files<CR>', '[P]ick [F]iles')
nmap('<leader>pg', '<CMD>Pick grep_live<CR>', '[P]ick [G]rep')
nmap('<leader>pb', '<CMD>Pick buffers<CR>', '[P]ick [B]uffers')
nmap('<leader><leader>', '<CMD>Pick commands<CR>', '[P]ick vim commands')

nmap('<leader>br', '<CMD>e %<CR>', '[B]uffer [R]efresh')
nmap('<leader>bd', '<CMD>bprevious<CR><CMD>bdelete #<CR>', '[B]uffer [R]efresh')

-- }}}
-- LSP {{{

vim.api.nvim_create_autocmd("LspAttach", {
    group = vim.api.nvim_create_augroup("lsp-attach-config", { clear = true }),
    callback = function(args)
        local bufnr = args.buf

        local buffer_nmap = function(keys, func, desc)
            vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc })
        end

        buffer_nmap("<leader>rn", vim.lsp.buf.rename, "[R]e[n]ame Symbol")
        buffer_nmap("<leader>ca", vim.lsp.buf.code_action, "[C]ode [A]ction")

        buffer_nmap("gd", vim.lsp.buf.definition, "[G]oto [D]efinition")
        buffer_nmap("ga", vim.lsp.buf.code_action, "Do code action")

        local hover_or_open_diagnostic_float = function()
            local lineNumber = vim.fn.line(".") - 1 -- Needs to be 0-based indexing but line returns 1 based
            local diag = vim.diagnostic.get(0, { lnum = lineNumber })
            local has_diagnostics = #diag > 0
            if has_diagnostics then
                vim.diagnostic.open_float()
            else
                vim.lsp.buf.hover()
            end
        end

        -- See `:help K` for why this keymap
        buffer_nmap("K", hover_or_open_diagnostic_float, "Hover Documentation")
        buffer_nmap("<C-h>", vim.lsp.buf.signature_help, "Signature Documentation")

        -- Lesser used LSP functionality
        buffer_nmap("gD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")

        vim.notify("LSP integration enabled.", vim.log.levels.DEBUG)
    end
})

-- Populate loclist with the current buffer diagnostics
vim.api.nvim_create_autocmd('DiagnosticChanged', {
    callback = function(args)
        vim.diagnostic.setloclist({ open = false })
    end,
})

-- }}}
-- Abbreviations {{{
local abbreviations = {
    teh = "the",
    becuase = "because",
    concats = "concatenates",
    editting = "editing",
    irreplacable = "irreplaceable",
    repos = "repositories",
    repo = "repository",
    similiar = "similar",
}

for abbreviation, expansion in pairs(abbreviations) do
    vim.cmd("abb " .. abbreviation .. " " .. expansion)
end
-- }}}
-- Auto commands {{{
-- Open location and quickfix list when using associated commands {{{
vim.cmd([[
augroup AutoOpenListWindow
    autocmd!
    autocmd QuickFixCmdPost [^l]* cwindow
    autocmd QuickFixCmdPost l*    lwindow
augroup END
]])
-- }}}
-- Highlight on yank {{{
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
    callback = function()
        vim.highlight.on_yank()
    end,
    group = highlight_group,
    pattern = "*",
})
-- }}}
-- Make Terminal more usable on startup {{{
vim.api.nvim_command([[
augroup TerminalSettings
    autocmd TermOpen * startinsert
    autocmd TermOpen * setlocal listchars= nonumber norelativenumber
augroup END
]])
-- }}}
-- }}}
vim.cmd("colorscheme challenger_deep")
