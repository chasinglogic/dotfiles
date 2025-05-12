return {
	{
		-- LSP Configuration & Plugins
		"neovim/nvim-lspconfig",
		dependencies = {
			-- Automatically install LSPs to stdpath for neovim
			"mason-org/mason.nvim",
			"mason-org/mason-lspconfig.nvim",

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
	}
}
