return {
	"folke/snacks.nvim",
	---@type snacks.Config
	opts = {
		bigfile = {},
		picker = {}
	},
	keys = {
		{ "<leader>ps",       function() Snacks.picker.git_grep({ untracked = true }) end,  desc = "Search Project" },
		{ "<leader>pf",       function() Snacks.picker.git_files({ untracked = true }) end, desc = "Project Files" },
		{ "<leader>bb",       function() Snacks.picker.buffers() end,                       desc = "Search Buffers" },
		{ "<leader><leader>", function() Snacks.picker.commands() end,                      desc = "Search Commands" },
		{ "<leader>sg",       function() Snacks.picker.git_log() end,                       desc = "Search Git Commits" },
		{ "<leader>sr",       function() Snacks.picker.lsp_references() end,                desc = "Search LSP References" },
		{ "<leader>sd",       function() Snacks.picker.lsp_definitions() end,               desc = "Search LSP Definitions" },
		{ "<leader>ss",       function() Snacks.picker.lsp_workspace_symbols() end,         desc = "Search LSP Symbols" },
	},
}
