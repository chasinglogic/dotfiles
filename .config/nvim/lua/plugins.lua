-- Install and Configure plugins
require("lazy").setup({
	-- Tpope goodness
	"tpope/vim-abolish",      -- Better subst
	"tpope/vim-commentary",   -- Commenting code
	"tpope/vim-surround",     -- Surrounding of text
	"tpope/vim-eunuch",       -- Useful commands like Rename, Delete, Move, SudoWrite
	"tpope/vim-fugitive",     -- Git integration
	"tpope/vim-endwise",      -- Automatically add end or similar constructs
	"tpope/vim-rsi",          -- Readline bindings in the vim command line
	"tpope/vim-sleuth",       -- Detect tabstop and shiftwidth automatically

	"gabrielpoca/replacer.nvim", -- Allows you to edit the quickfix window
	"folke/which-key.nvim",   -- Help me remember keybinds
	-- Linting when there is no Language Server or when it complements the
	-- Language Server
	"mfussenegger/nvim-lint",

	-- Show indentation guides
	{ "lukas-reineke/indent-blankline.nvim", main = "ibl",          opts = {} },

	-- Better netrw
	{ "stevearc/oil.nvim",                   opts = {} },

	-- Auto pair things
	{ "windwp/nvim-autopairs",               event = "InsertEnter", opts = {} },

	-- "gc" to comment visual regions/lines
	{ "numToStr/Comment.nvim",               opts = {} },

	{
		"elixir-tools/elixir-tools.nvim",
		version = "*",
		event = { "BufReadPre", "BufNewFile" },
		config = function()
			local elixir = require("elixir")
			local elixirls = require("elixir.elixirls")

			elixir.setup {
				nextls = { enable = true },
				-- Disabled because nextls works with credo and ends up doubling
				-- diagnostics if both are enabled.
				credo = { enable = false },
				elixirls = {
					enable = true,
					tag = "v0.20.0",
					settings = elixirls.settings {
						dialyzerEnabled = false,
						enableTestLenses = true,
						fetchDeps = true,
						suggestSpects = true,
					},
					on_attach = function(client, bufnr)
						vim.keymap.set("n", "<space>mf", ":ElixirFromPipe<cr>", { buffer = true, noremap = true })
						vim.keymap.set("n", "<space>mp", ":ElixirToPipe<cr>", { buffer = true, noremap = true })
						vim.keymap.set("v", "<space>me", ":ElixirExpandMacro<cr>", { buffer = true, noremap = true })
					end,
				}
			}
		end,
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
	},

	-- NOTE: This is where your plugins related to LSP can be installed.
	{
		-- LSP Configuration & Plugins
		"neovim/nvim-lspconfig",
		dependencies = {
			-- Automatically install LSPs to stdpath for neovim
			"williamboman/mason.nvim",
			"williamboman/mason-lspconfig.nvim",

			-- Add LSP diagnostics to the location and other useful integration
			"folke/trouble.nvim",

			-- Useful status updates for LSP
			-- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
			{ "j-hui/fidget.nvim", opts = {} },

			-- Additional lua configuration, makes nvim stuff amazing!
			"folke/neodev.nvim",

			-- Better tsserver integration
			-- "pmizio/typescript-tools.nvim",
		},
	},

	{
		-- Snippet Engine
		"L3MON4D3/LuaSnip",
		version = "v2.*",
		build = "make install_jsregexp",
		config = function()
			require("luasnip.loaders.from_snipmate").load()
			require("luasnip.loaders.from_lua").load()
		end,
	},

	{
		-- Autocompletion
		"hrsh7th/nvim-cmp",
		dependencies = {
			-- autocompletion for snippets
			"saadparwaiz1/cmp_luasnip",

			-- Adds LSP completion capabilities
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer", -- Autocomplete based on words in buffer
			"hrsh7th/cmp-path", -- Autocomplete filepaths
		},
	},

	-- NOTE: color theme
	{
		'dracula/vim',
		priority = 1000,
		config = function()
			vim.cmd.colorscheme("dracula")
		end,
	},
	-- This is a good theme, might go back to it
	-- {
	-- 	'folke/tokyonight.nvim',
	-- 	priority = 1000,
	-- 	config = function()
	-- 		vim.cmd.colorscheme("tokyonight-storm")
	-- 	end,
	-- },

	{
		-- Set lualine as statusline
		'nvim-lualine/lualine.nvim',
		-- See `:help lualine.txt`
		opts = {
			options = {
				icons_enabled = false,
				theme = 'dracula',
				component_separators = '|',
				section_separators = '',
			},
		},
	},

	-- Fuzzy Finder (files, lsp, etc)
	{
		"nvim-telescope/telescope.nvim",
		branch = "0.1.x",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope-live-grep-args.nvim",
			-- Fuzzy Finder Algorithm which requires local dependencies to be built.
			-- Only load if `make` is available.
			{
				"nvim-telescope/telescope-fzf-native.nvim",
				build = "make",
				cond = function()
					return vim.fn.executable("make") == 1
				end,
			},
		},
	},

	{
		-- Highlight, edit, and navigate code
		"nvim-treesitter/nvim-treesitter",
		dependencies = {
			"nvim-treesitter/nvim-treesitter-textobjects",
			-- Autoclose languges which use "end" like Ruby and Elixir.
			"RRethy/nvim-treesitter-endwise",

			-- Auto close and rename JSX/html tags
			"windwp/nvim-ts-autotag",
		},
		build = ":TSUpdate",

	},

	-- Justfile syntax support
	{
		"NoahTheDuke/vim-just",
		ft = { "just" },
	},

	require("chasinglogic.plugins.autoformat"),
}, {})
