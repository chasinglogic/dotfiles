return {
	{
		-- Highlight, edit, and navigate code
		"nvim-treesitter/nvim-treesitter",
		dependencies = {
			"nvim-treesitter/nvim-treesitter-textobjects",
			-- Autoclose languges which use "end" like Ruby and Elixir.
			"RRethy/nvim-treesitter-endwise",

			"nushell/tree-sitter-nu",

			-- Auto close and rename JSX/html tags
			"windwp/nvim-ts-autotag",
		},
		build = ":TSUpdate",

	}
}
