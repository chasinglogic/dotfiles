return {
	{
		-- Snippet Engine
		"L3MON4D3/LuaSnip",
		version = "v2.*",
		build = "make install_jsregexp",
		config = function()
			require("luasnip.loaders.from_snipmate").load()
			require("luasnip.loaders.from_lua").load()
		end,
	}
}
