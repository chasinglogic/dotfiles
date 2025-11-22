-- File tree
return {
	{
		"nvim-neo-tree/neo-tree.nvim",
		branch = "v3.x",
		config = function()
			require('neo-tree').setup({
				filesystem = {
					hijack_netrw_behavior = "disabled",
					filtered_items = {
						visible = false,
						hide_dotfiles = false,
						hide_hidden = false,
						hide_gitignored = true,
						hide_by_name = {
							"node_modules",
						},
					}
				}
			})
		end,
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-tree/nvim-web-devicons",
			"MunifTanjim/nui.nvim",
		}
	},
}
