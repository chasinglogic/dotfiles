-- vim: foldmethod=marker

-- Options {{{
-- Enable modelines
vim.o.modeline = true
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
-- Limit text to 80 columns
vim.o.textwidth = 80
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
-- Remove underscore as a keyword separator so the word textobject treats
-- each_word_as_separate
vim.o.iskeyword = '@,48-57,192-255'
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
-- nosort   Disable sorting of completion candidates based on fuzzy
--	        scores when "fuzzy" is enabled.  Candidates will appear
--	        in their original order.
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
vim.o.completeopt = 'menuone,fuzzy,nosort,noselect'
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
-- }}}
-- Plugins {{{

vim.pack.add({
    -- Color theme
    { src = "https://github.com/catppuccin/nvim" },
    -- LSP helpers
    { src = "https://github.com/neovim/nvim-lspconfig" },
    { src = "https://github.com/mason-org/mason.nvim" },
    { src = "https://github.com/mason-org/mason-lspconfig.nvim" },
    { src = "https://github.com/WhoIsSethDaniel/mason-tool-installer.nvim" },
    -- One plugin to rule them all (provides tons of basic functionality)
    { src = "https://github.com/nvim-mini/mini.nvim",                      version = 'main' },
    -- Pre-configured treesitter setups with installation
    { src = "https://github.com/nvim-treesitter/nvim-treesitter",          version = 'main' },
    -- Autoformatting that uses tools and LSP together happily.
    { src = "https://github.com/stevearc/conform.nvim" },
    -- Loads of free snippets
    { src = "https://github.com/rafamadriz/friendly-snippets" },
    -- Manage files in neovim without suffering
    { src = "https://github.com/stevearc/oil.nvim" },
    -- Give contextual location information in a winbar
    { src = "https://github.com/Bekaboo/dropbar.nvim" },
    -- Do git stuff with vim, like magit
    { src = "https://github.com/tpope/vim-fugitive" },
    -- Automatically add end or similar constructs
    { src = "https://github.com/tpope/vim-endwise" },
    -- Useful commands like Rename, Delete, SudoWrite
    { src = "https://github.com/tpope/vim-eunuch" },
    -- Help me remember keybinds
    { src = "https://github.com/folke/which-key.nvim" },
}, { confirm = false })
-- Which Key {{{
require('which-key').setup()
-- }}}
-- DropBar {{{
require('dropbar').setup()
-- }}}
-- TreeSitter {{{
local treesitter_langs = {
    "bash",
    "c",
    "cpp",
    "css",
    "dockerfile",
    "embedded_template",
    "go",
    "gotmpl",
    "hcl",
    "html",
    "javascript",
    "jsdoc",
    "json",
    "jsonc",
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
    "yaml",
    "zig",
}
require('nvim-treesitter').install(treesitter_langs)
vim.api.nvim_create_autocmd('FileType', {
    pattern = treesitter_langs,
    callback = function()
        vim.treesitter.start()
        vim.wo.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
        vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
    end,
})
-- }}}
-- Mini {{{
require('mini.pairs').setup()
require('mini.surround').setup()
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
require('mini.completion').setup()
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
        "docker-compose-language-service",
        "dockerfile-language-server",
        "erb-formatter",
        "erb-lint",
        "eslint_d",
        "gopls",
        "helm-ls",
        "html-lsp",
        "lua_ls",
        "pyright",
        "rubocop",
        "ruby-lsp",
        "ruff",
        "rust-analyzer",
        "stimulus-language-server",
        "shellcheck",
        "shfmt",
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
-- }}}
-- Keys {{{

local nmap = function(keys, func, desc)
    vim.keymap.set("n", keys, func, { desc = desc })
end

vim.keymap.set('i', 'fd', '<ESC>')
--On MacOS I tell my keyboard to use alt as meta as the Lord intended but that
--means I can't type the hash symbol on my UK keyboards. This fixes that.
if vim.fn.has("macunix") then
    vim.keymap.set("i", "<M-3>", "#")
end

nmap("-", "<CMD>Oil<CR>", "Open parent directory")
nmap("!", ":!", "Run shell command")

nmap('<leader>?', function()
    require("which-key").show({ global = false })
end, "Buffer local keymaps")

nmap('<leader>fs', '<CMD>w<CR>', '[F]ile [S]ave')
nmap('<leader>fq', '<CMD>wq<CR>', '[F]ile save and [Q]uit')

nmap('<leader>w', '<C-w>', '[W]indows')
nmap('<leader>r', '<CMD>source ~/.config/nvim/init.lua<CR>')
nmap('<leader>u', vim.pack.update)

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
vim.cmd("colorscheme catppuccin-mocha")
