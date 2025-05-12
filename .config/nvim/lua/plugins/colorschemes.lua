return {
	-- NOTE: color theme
	-- {
	-- 	'dracula/vim',
	-- 	priority = 1000,
	-- },
	{
		"Mofiqul/dracula.nvim",
		priority = 1000,
	},
	{
		"folke/tokyonight.nvim",
		priority = 1000,
	},
	{
		"catppuccin/nvim",
		name = "catppuccin",
		priority = 1000,
		config = function()
			require('catppuccin').setup({})
			vim.o.background = 'dark'
			vim.cmd.colorscheme('catppuccin-mocha')
		end
	},
}
