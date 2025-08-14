return {
	"ibhagwan/fzf-lua",
	-- optional for icon support
	dependencies = { "nvim-tree/nvim-web-devicons" },
	-- or if using mini.icons/mini.nvim
	-- dependencies = { "echasnovski/mini.icons" },
	opts = {
		"ivy",
		files  = {
			previewer = false,
		},
		grep   = {
			rg_opts = "--hidden --column --line-number --no-heading --color=always --smart-case --max-columns=4096 -e",
		},
		keymap = {
			builtin = {
				["<F1>"]     = "toggle-help",
				["<F2>"]     = "toggle-fullscreen",
				-- Only valid with the 'builtin' previewer
				["<F3>"]     = "toggle-preview-wrap",
				["<F4>"]     = "toggle-preview",
				["<F5>"]     = "toggle-preview-ccw",
				["<F6>"]     = "toggle-preview-cw",
				["<C-d>"]    = "preview-page-down",
				["<C-u>"]    = "preview-page-up",
				["<S-left>"] = "preview-page-reset",
			},
			fzf = {
				["ctrl-z"] = "abort",
				["ctrl-f"] = "half-page-down",
				["ctrl-b"] = "half-page-up",
				["ctrl-a"] = "beginning-of-line",
				["ctrl-e"] = "end-of-line",
				["alt-a"]  = "toggle-all",
				-- Only valid with fzf previewers (bat/cat/git/etc)
				["f3"]     = "toggle-preview-wrap",
				["f4"]     = "toggle-preview",
				["ctrl-d"] = "preview-page-down",
				["ctrl-u"] = "preview-page-up",
				["ctrl-q"] = "select-all+accept",
			},
		},
	},
	keys = {
		{ "<leader>ps", "<cmd>FzfLua live_grep<cr>", desc = "Search Project" },
		{ "<leader>pf", "<cmd>FzfLua files<cr>",     desc = "Project Files" },
		{ "<leader>bb", "<cmd>FzfLua buffers<cr>",   desc = "Search Buffers" },
	},
}
