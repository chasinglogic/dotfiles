local cmp = require("cmp")
local snippy = require("snippy")

cmp.setup({
	completion = {
		keyword_length = 3,
	},
	snippet = {
		expand = function(args)
			require("snippy").expand_snippet(args.body)
		end,
	},
	preselect = cmp.PreselectMode.None,
	mapping = {
		-- ["<Tab>"] = cmp.mapping(function(fallback)
		--   if cmp.visible() then
		--     cmp.confirm({ select = true })
		--   else
		--     fallback()
		--   end
		-- end, { "i", "s" }),
		["<Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.confirm({ select = true })
			elseif snippy.can_expand_or_advance() then
				snippy.expand_or_advance()
			else
				fallback()
			end
		end, { "i", "s" }),

		["<S-Tab>"] = cmp.mapping(function(fallback)
			if snippy.can_jump(-1) then
				snippy.previous()
			else
				fallback()
			end
		end, { "i", "s" }),
		["<C-n>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			else
				fallback()
			end
		end, { "i", "s" }),
		["<C-p>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_prev_item()
			else
				fallback()
			end
		end, { "i", "s" }),
		["<C-d>"] = cmp.mapping.scroll_docs(-4),
		["<C-f>"] = cmp.mapping.scroll_docs(4),
		["<C-Space>"] = cmp.mapping.complete(),
		["<C-e>"] = cmp.mapping.close(),
		["<CR>"] = cmp.mapping.confirm({ select = false }),
	},
	sources = {
		{ name = "nvim_lsp", priority = 1000 },
		{ name = "buffer", priority = 500 },
		{ name = "path", priority = 250 },
		{ name = "snippy", priority = 100 },
	},
})

-- These filetypes have broken completion with the cmp.confirm binding above. So
-- this uses a more "simple" binding which just selects completions instead.
-- Leaving the default behaviour above tho because most LSPs will auto populate
-- imports with cmp.confirm but not cmp.select_next_item
local busted_languages = { "go" }
for _, filetype in ipairs(busted_languages) do
	cmp.setup.filetype(filetype, {
		mapping = {
			["<Tab>"] = cmp.mapping(function(fallback)
				if cmp.visible() then
					cmp.select_next_item()
				else
					fallback()
				end
			end, { "i", "s" }),
		},
	})
end

local cmp_autopairs = require("nvim-autopairs.completion.cmp")
cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())
