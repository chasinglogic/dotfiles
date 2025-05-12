return {
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
	}
}
