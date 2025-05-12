-- Fuzzy Finder (files, lsp, etc)
return {
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
}
