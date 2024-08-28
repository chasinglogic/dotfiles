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

	"AndrewRadev/splitjoin.vim", -- Switch between single-line and multiline forms of code

	"gabrielpoca/replacer.nvim", -- Allows you to edit the quickfix window
	-- Linting when there is no Language Server or when it complements the
	-- Language Server
	"mfussenegger/nvim-lint",

	-- Like avy from Emacs. Jump to stuff on screen.
	{
		"ggandor/leap.nvim",
		config = function()
			-- default mappings conflicts with vim-surround so use custom ones.
			vim.keymap.set({ 'n', 'x', 'o' }, '<leader>jc', '<Plug>(leap-forward)')
			vim.keymap.set({ 'n', 'x', 'o' }, '<leader>jC', '<Plug>(leap-backward)')
			vim.keymap.set({ 'n', 'x', 'o' }, '<leader>jw', '<Plug>(leap-from-window)')
		end
	},

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
						vim.keymap.set("n", "<space>mf", ":ElixirFromPipe<cr>",
							{ buffer = true, noremap = true })
						vim.keymap.set("n", "<space>mp", ":ElixirToPipe<cr>",
							{ buffer = true, noremap = true })
						vim.keymap.set("v", "<space>me", ":ElixirExpandMacro<cr>",
							{ buffer = true, noremap = true })
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
	},
	{
		"folke/tokyonight.nvim",
		lazy = false,
		priority = 1000,
		opts = {},
		config = function()
			vim.o.background = 'dark'
			vim.cmd.colorscheme('tokyonight')
		end
	},

	{
		-- Set lualine as statusline
		'nvim-lualine/lualine.nvim',
		-- See `:help lualine.txt`
		opts = {
			options = {
				icons_enabled = false,
				theme = 'auto',
				component_separators = '|',
				section_separators = '',
			},
		},
	},

	-- Fuzzy Finder (files, lsp, etc)
	{
		"nvim-telescope/telescope.nvim",
		branch = "0.1.x",
		config = function()
			require("telescope").setup({
				defaults = require('telescope.themes').get_ivy(),
			})
		end,
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

	-- Create lists of buffers and quickly jump to them. Also they are saved and
	-- restored automatically so that's cool.
	{
		"ThePrimeagen/harpoon",
		branch = "harpoon2",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope.nvim"
		},
		config = function()
			local harpoon = require('harpoon')
			-- REQUIRED
			harpoon:setup()
			-- REQUIRED

			vim.keymap.set("n", "<leader>a", function() harpoon:list():add() end)
			vim.keymap.set("n", "<C-e>", function() harpoon.ui:toggle_quick_menu(harpoon:list()) end)

			vim.keymap.set("n", "<C-1>", function() harpoon:list():select(1) end)
			vim.keymap.set("n", "<C-2>", function() harpoon:list():select(2) end)
			vim.keymap.set("n", "<C-3>", function() harpoon:list():select(3) end)
			vim.keymap.set("n", "<C-4>", function() harpoon:list():select(4) end)

			-- Toggle previous & next buffers stored within Harpoon list
			vim.keymap.set("n", "<C-[>", function() harpoon:list():prev() end)
			vim.keymap.set("n", "<C-]>", function() harpoon:list():next() end)
		end
	},

	require("chasinglogic.plugins.autoformat"),
}, {})
