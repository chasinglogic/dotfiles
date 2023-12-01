-- Install and Configure plugins
require("lazy").setup({
	-- Tpope goodness
	"tpope/vim-abolish",      -- Better subst
	"tpope/vim-vinegar",      -- Netrw improvements
	"tpope/vim-commentary",   -- Commenting code
	"tpope/vim-surround",     -- Surrounding of text
	"tpope/vim-eunuch",       -- Useful commands like Rename, Delete, Move, SudoWrite
	"tpope/vim-fugitive",     -- Git integration
	"tpope/vim-endwise",      -- Automatically add end or similar constructs
	"tpope/vim-rsi",          -- Readline bindings in the vim command line
	"tpope/vim-sleuth",       -- Detect tabstop and shiftwidth automatically

	"gabrielpoca/replacer.nvim", -- Allows you to edit the quickfix window
	"folke/which-key.nvim",   -- Help me remember keybinds

	"windwp/nvim-autopairs",  -- Auto pair things
	"windwp/nvim-ts-autotag", -- Auto close and rename JSX/html tags

	-- "gc" to comment visual regions/lines
	{ "numToStr/Comment.nvim", opts = {} },

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
		-- Autocompletion
		"hrsh7th/nvim-cmp",
		dependencies = {
			-- Snippet Engine & its associated nvim-cmp source
			"L3MON4D3/LuaSnip",
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
		},
		build = ":TSUpdate",
	},

	require("chasinglogic.plugins.autoformat"),
}, {})
