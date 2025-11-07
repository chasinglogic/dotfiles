-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Install `lazy.nvim` plugin manager
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

vim.keymap.set("i", "fd", "<ESC>")

-- Allows filtering down the quickfix and location lists
vim.cmd.packadd('cfilter')

-- Load plugins
require("lazy").setup({
  spec = {
    { import = "plugins" },
  },
  checker = {
    enabled = true,
    notify = false,
  },
  change_detection = {
    enabled = false,
    notify = false,
  },
})

require("chasinglogic.keys")
require("chasinglogic.editor")
require("chasinglogic.abbrevs")

-- vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = "*",
})

-- [[ Configure Treesitter ]]
-- See `:help nvim-treesitter`
-- Defer Treesitter setup after first render to improve startup time of 'nvim {filename}'
vim.defer_fn(function()
  local parser_config = require 'nvim-treesitter.parsers'.get_parser_configs()
  parser_config.gotmpl = {
    install_info = {
      url = "https://github.com/ngalaiko/tree-sitter-go-template",
      files = { "src/parser.c" }
    },
    filetype = "gotmpl",
    used_by = { "gohtmltmpl", "gotexttmpl", "gotmpl", "yaml", "html" }
  }

  require("nvim-treesitter.configs").setup({
    -- Add languages to be installed here that you want installed for treesitter
    ensure_installed = {
      "bash",
      "c",
      "cpp",
      "css",
      "dockerfile",
      "eex",
      "elixir",
      "go",
      "gotmpl",
      "haskell",
      "heex",
      "html",
      "javascript",
      "jsdoc",
      "json",
      "jsonc",
      "kdl",
      "lua",
      "php",
      "python",
      "r",
      "ruby",
      "rust",
      "svelte",
      "toml",
      "tsx",
      "typescript",
      "vala",
      "vim",
      "vimdoc",
      "vue",
      "yaml",
      "zig",
    },

    -- Autoinstall languages that are not installed.
    auto_install = true,

    endwise = { enable = true },
    highlight = { enable = true },
    indent = { enable = true },
    incremental_selection = {
      enable = true,
      keymaps = {
        init_selection = "<c-space>",
        node_incremental = "<c-space>",
        scope_incremental = "<c-s>",
        node_decremental = "<M-space>",
      },
    },
    textobjects = {
      select = {
        enable = true,
        lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
        keymaps = {
          -- You can use the capture groups defined in textobjects.scm
          ["aa"] = "@parameter.outer",
          ["ia"] = "@parameter.inner",
          ["af"] = "@function.outer",
          ["if"] = "@function.inner",
          ["ac"] = "@class.outer",
          ["ic"] = "@class.inner",
        },
      },
      move = {
        enable = true,
        set_jumps = true, -- whether to set jumps in the jumplist
        goto_next_start = {
          ["]m"] = "@function.outer",
          ["]]"] = "@class.outer",
        },
        goto_next_end = {
          ["]M"] = "@function.outer",
          ["]["] = "@class.outer",
        },
        goto_previous_start = {
          ["[m"] = "@function.outer",
          ["[["] = "@class.outer",
        },
        goto_previous_end = {
          ["[M"] = "@function.outer",
          ["[]"] = "@class.outer",
        },
      },
      swap = {
        enable = true,
        swap_next = {
          ["<leader>a"] = "@parameter.inner",
        },
        swap_previous = {
          ["<leader>A"] = "@parameter.inner",
        },
      },
    },
  })

  require('nvim-ts-autotag').setup()
end, 0)

-- [[ Configure LSP ]]
--  This function gets run when an LSP connects to a particular buffer.
local on_attach = function(_, bufnr)
  -- NOTE: Remember that lua is a real programming language, and as such it is possible
  -- to define small helper and utility functions so you don't have to repeat yourself
  -- many times.
  --
  -- In this case, we create a function that lets us more easily define mappings specific
  -- for LSP related items. It sets the mode, buffer and description for us each time.
  local nmap = function(keys, func, desc)
    if desc then
      desc = "LSP: " .. desc
    end

    vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc })
  end

  nmap("<leader>rn", vim.lsp.buf.rename, "[R]e[n]ame Symbol")
  nmap("<leader>ca", vim.lsp.buf.code_action, "[C]ode [A]ction")

  nmap("gd", vim.lsp.buf.definition, "[G]oto [D]efinition")
  nmap("ga", vim.lsp.buf.code_action, "Do code action")

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
  nmap("K", hover_or_open_diagnostic_float, "Hover Documentation")
  nmap("<C-h>", vim.lsp.buf.signature_help, "Signature Documentation")

  -- Lesser used LSP functionality
  nmap("gD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, "Format", function(_)
    vim.lsp.buf.format()
  end, { desc = "Format current buffer with LSP" })

  nmap("<leader>bf", vim.lsp.buf.format, "[B]uffer [F]ormat")
end

-- Enable the following language servers
--  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
--
--  Add any additional override configuration in the following tables. They will be passed to
--  the `settings` field of the server config. You must look up that documentation yourself.
--
--  If you want to override the default filetypes that your language server will attach to you can
--  define the property 'filetypes' to the map in question.
local servers = {
  ansiblels = {},
  clangd = {},
  docker_compose_language_service = {},
  dockerls = {},
  elixirls = {},
  lexical = {},
  gopls = {},
  helm_ls = {},
  html = { filetypes = { 'html', 'twig', 'hbs' } },
  -- nginx_language_server = {},
  pyright = {},
  ruff = {},
  rust_analyzer = {},
  stimulus_ls = {},
  svelte = {},
  terraformls = {
    filetypes = { 'terraform', 'terraform-vars', 'hcl' },
  },
  tflint = {},
  ts_ls = {},
  zls = {},

  lua_ls = {
    Lua = {
      workspace = { checkThirdParty = false },
    },
  },

  -- nil_ls = {},
}

-- Setup neovim lua configuration
require("neodev").setup()

-- Ensure the servers above are installed
local mason_lspconfig = require("mason-lspconfig")

mason_lspconfig.setup({
  ensure_installed = vim.tbl_keys(servers),
})

vim.lsp.config('*', {
  on_attach = on_attach,
})

for server, settings in pairs(servers) do
  if not rawequal(next(settings), nil) then
    local filetypes = settings.filetypes
    settings.filetypes = nil

    vim.lsp.config(server, {
      settings = settings,
      filetypes = filetypes,
      on_attach = on_attach,
    })
  end
end

-- Populate loclist with the current buffer diagnostics
vim.api.nvim_create_autocmd('DiagnosticChanged', {
  callback = function(args)
    vim.diagnostic.setloclist({ open = false })
  end,
})

-- [[ Configure luasnip ]]
-- See `:help luasnip`
local luasnip = require("luasnip")
require("luasnip.loaders.from_vscode").lazy_load()
luasnip.config.setup({})

-- [[ Configure Oil ]]
-- See `:help oil`
local oil = require("oil")
oil.setup({
  -- Oil will take over directory buffers (e.g. `vim .` or `:e src/`)
  -- Set to false if you still want to use netrw.
  default_file_explorer = true,

  -- Id is automatically added at the beginning, and name at the end
  -- See :help oil-columns
  columns = {
    "icon",
    "permissions",
    "size",
    "mtime",
  },

  -- Buffer-local options to use for oil buffers
  buf_options = {
    buflisted = false,
    bufhidden = "hide",
  },

  -- Window-local options to use for oil buffers
  win_options = {
    wrap = false,
    signcolumn = "no",
    cursorcolumn = false,
    foldcolumn = "0",
    spell = false,
    list = false,
    conceallevel = 3,
    concealcursor = "nvic",
  },

  -- Send deleted files to the trash instead of permanently deleting them (:help oil-trash)
  delete_to_trash = false,

  -- Skip the confirmation popup for simple operations
  skip_confirm_for_simple_edits = false,

  -- Selecting a new/moved/renamed file or directory will prompt you to save changes first
  prompt_save_on_select_new_entry = true,

  -- Oil will automatically delete hidden buffers after this delay
  -- You can set the delay to false to disable cleanup entirely
  -- Note that the cleanup process only starts when none of the oil buffers are currently displayed
  cleanup_delay_ms = 2000,

  -- Keymaps in oil buffer. Can be any value that `vim.keymap.set` accepts OR a table of keymap
  -- options with a `callback` (e.g. { callback = function() ... end, desc = "", mode = "n" })
  -- Additionally, if it is a string that matches "actions.<name>",
  -- it will use the mapping at require("oil.actions").<name>
  -- Set to `false` to remove a keymap
  -- See :help oil-actions for a list of all available actions
  keymaps = {
    ["g?"] = "actions.show_help",
    ["<CR>"] = "actions.select",
    ["<C-s>"] = "actions.select_vsplit",
    ["<C-h>"] = "actions.select_split",
    ["<C-t>"] = "actions.select_tab",
    ["<C-p>"] = "actions.preview",
    ["<C-c>"] = "actions.close",
    ["<C-l>"] = "actions.refresh",
    ["."] = "actions.open_cmdline",
    ["-"] = "actions.parent",
    ["_"] = "actions.open_cwd",
    ["`"] = "actions.cd",
    ["~"] = "actions.tcd",
    ["gs"] = "actions.change_sort",
    ["gx"] = "actions.open_external",
    ["g."] = "actions.toggle_hidden",
    ["g\\"] = "actions.toggle_trash",
    ["gq"] = "actions.close",
  },

  -- Set to false to disable all of the above keymaps
  use_default_keymaps = true,

  view_options = {
    -- Show files and directories that start with "."
    show_hidden = true,

    -- This function defines what is considered a "hidden" file
    is_hidden_file = function(name, bufnr)
      return vim.startswith(name, ".")
    end,

    -- This function defines what will never be shown, even when `show_hidden` is set
    is_always_hidden = function(name, bufnr)
      return false
    end,

    sort = {
      -- sort order can be "asc" or "desc"
      -- see :help oil-columns to see which columns are sortable
      { "type", "asc" },
      { "name", "asc" },
    },
  },

  -- Configuration for the floating window in oil.open_float
  float = {
    -- Padding around the floating window
    padding = 2,
    max_width = 0,
    max_height = 0,
    border = "rounded",
    win_options = {
      winblend = 0,
    },

    -- This is the config that will be passed to nvim_open_win.
    -- Change values here to customize the layout
    override = function(conf)
      return conf
    end,
  },

  -- Configuration for the actions floating preview window
  preview = {
    -- Width dimensions can be integers or a float between 0 and 1 (e.g. 0.4 for 40%)
    -- min_width and max_width can be a single value or a list of mixed integer/float types.
    -- max_width = {100, 0.8} means "the lesser of 100 columns or 80% of total"
    max_width = 0.9,

    -- min_width = {40, 0.4} means "the greater of 40 columns or 40% of total"
    min_width = { 40, 0.4 },

    -- optionally define an integer/float for the exact width of the preview window
    width = nil,

    -- Height dimensions can be integers or a float between 0 and 1 (e.g. 0.4 for 40%)
    -- min_height and max_height can be a single value or a list of mixed integer/float types.
    -- max_height = {80, 0.9} means "the lesser of 80 columns or 90% of total"
    max_height = 0.9,

    -- min_height = {5, 0.1} means "the greater of 5 columns or 10% of total"
    min_height = { 5, 0.1 },

    -- optionally define an integer/float for the exact height of the preview window
    height = nil,
    border = "rounded",
    win_options = {
      winblend = 0,
    },
  },

  -- Configuration for the floating progress window
  progress = {
    max_width = 0.9,
    min_width = { 40, 0.4 },
    width = nil,
    max_height = { 10, 0.9 },
    min_height = { 5, 0.1 },
    height = nil,
    border = "rounded",
    minimized_border = "none",
    win_options = {
      winblend = 0,
    },
  },
})
vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })

-- [[ Configure terminal ]]
vim.api.nvim_command("augroup TerminalSettings")
vim.api.nvim_command("autocmd TermOpen * startinsert")
vim.api.nvim_command("autocmd TermOpen * setlocal listchars= nonumber norelativenumber")
vim.api.nvim_command("augroup END")

-- [[ Configure nvim-lint ]]
require('lint').linters_by_ft = {
  -- markdown = { 'proselint', },
  sh = { 'shellcheck', },
  zsh = { 'shellcheck', },
  bash = { 'shellcheck', },
  nix = { 'nix', },
}

vim.api.nvim_create_autocmd({ "BufWritePost" }, {
  callback = function()
    require("lint").try_lint()
  end,
})

-- vim: ts=2 sts=2 sw=2 et
