-- Use the language server to automatically format your code on save.
-- Adds additional commands as well to manage the behavior

return {
	"neovim/nvim-lspconfig",
	dependencies = {
		"mhartington/formatter.nvim",
	},
	config = function()
		require("formatter").setup({
			logging = true,
			log_level = vim.log.levels.WARN,
			filetype = {
				python = {
					require("formatter.filetypes.python").black,
					require("formatter.filetypes.python").isort,
				}
			}
		})

		-- Switch for controlling whether you want autoformatting.
		--  Use :AutoformatToggle to toggle autoformatting on or off
		local format_is_enabled = true
		vim.api.nvim_create_user_command("AutoformatToggle", function()
			format_is_enabled = not format_is_enabled
			print("Setting autoformatting to: " .. tostring(format_is_enabled))
		end, {})

		-- Create an augroup that is used for managing our formatting autocmds.
		--      We need one augroup per client to make sure that multiple clients
		--      can attach to the same buffer without interfering with each other.
		local _augroups = {}
		local get_augroup = function(client)
			if not _augroups[client.id] then
				local group_name = "chasinglogic-lsp-format-" .. client.name
				local id = vim.api.nvim_create_augroup(group_name, { clear = true })
				_augroups[client.id] = id
			end

			return _augroups[client.id]
		end

		-- A list of clients that don't work well with autoformatting
		local banned_clients = {
			["ruff_lsp"] = true,
		}

		-- Whenever an LSP attaches to a buffer, we will run this to setup the
		-- augroup for autoformatting.
		--
		-- See `:help LspAttach` for more information about this autocmd event.
		vim.api.nvim_create_autocmd("LspAttach", {
			group = vim.api.nvim_create_augroup("chasinglogic-lsp-attach-format", { clear = true }),
			-- This is where we attach the autoformatting for reasonable clients
			callback = function(args)
				local client_id = args.data.client_id
				local client = vim.lsp.get_client_by_id(client_id)
				local bufnr = args.buf
				local use_lsp_formatting = client.server_capabilities.documentFormattingProvider and
					not banned_clients[client.name]

				-- Create an autocmd that will run *before* we save the buffer.
				--  Run the formatting command for the LSP that has just attached.
				vim.api.nvim_create_autocmd("BufWritePre", {
					group = get_augroup(client),
					buffer = bufnr,
					callback = function()
						if not format_is_enabled then
							return
						end

						if not use_lsp_formatting then
							vim.api.nvim_command("FormatWrite")
							return
						end

						-- This exists so that you get a 'goimports' like
						-- autoformat experience. See:
						-- https://github.com/golang/tools/blob/master/gopls/doc/vim.md#neovim-imports
						if client.name == 'gopls' then
							local params = vim.lsp.util.make_range_params()
							params.context = { only = { "source.organizeImports" } }
							-- buf_request_sync defaults to a 1000ms timeout. Depending on your
							-- machine and codebase, you may want longer. Add an additional
							-- argument after params if you find that you have to write the file
							-- twice for changes to be saved.
							-- E.g., vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, 3000)
							local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params)
							for cid, res in pairs(result or {}) do
								for _, r in pairs(res.result or {}) do
									if r.edit then
										local enc = (vim.lsp.get_client_by_id(cid) or {}).offset_encoding or "utf-16"
										vim.lsp.util.apply_workspace_edit(r.edit, enc)
									end
								end
							end
						end

						vim.lsp.buf.format({
							async = false,
							filter = function(c)
								return c.id == client.id
							end,
						})
					end,
				})
			end,
		})
	end,
}
