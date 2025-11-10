return {
	-- NOTE: color theme
	-- {
	-- 	'dracula/vim',
	-- 	priority = 1000,
	-- },
	{
		"maxmx03/dracula.nvim",
		priority = 1000,
		opts = {},
	},
	{
		"folke/tokyonight.nvim",
		priority = 1000,
		opts = {},
		-- config = function()
		-- 	vim.cmd.colorscheme('tokyonight-day')
		-- end
	},
	{
		"miikanissi/modus-themes.nvim",
		priority = 1000,
	},
	{
		"catppuccin/nvim",
		name = "catppuccin",
		priority = 1000,
		opts = {},
		config = function()
			vim.cmd.colorscheme('catppuccin-mocha')
		end
	},
}
