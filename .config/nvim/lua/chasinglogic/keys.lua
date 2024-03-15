--Files {{{
vim.keymap.set("n", "<leader>fs", ":w<cr>")
vim.keymap.set("n", "<leader>fq", ":wq<cr>")
vim.keymap.set("n", "<leader>fa", ":wa<cr>")
--}}}
--Project level ops {{{
vim.keymap.set("n", "<leader>pf", ":Telescope find_files previewer=false hidden=true<cr>")
vim.keymap.set("n", "<leader>ps", ":Telescope live_grep<cr>")
vim.keymap.set("n", "<leader>pS", require("telescope").extensions.live_grep_args.live_grep_args)
vim.keymap.set("n", "<leader>pc", ":make<cr>")
vim.keymap.set("n", "<leader>h", function()
	require("replacer").run({ rename_files = false })
end)
vim.keymap.set("n", "<leader>H", function()
	require("replacer").run({ rename_files = true })
end)
-- vim.keymap.set("n", "<leader>gf", require("telescope.builtin").git_files, { desc = "Search [G]it [F]iles" })
-- vim.keymap.set("n", "<leader>sf", require("telescope.builtin").find_files, { desc = "[S]earch [F]iles" })
-- vim.keymap.set("n", "<leader>sh", require("telescope.builtin").help_tags, { desc = "[S]earch [H]elp" })
-- vim.keymap.set("n", "<leader>sw", require("telescope.builtin").grep_string, { desc = "[S]earch current [W]ord" })
-- vim.keymap.set("n", "<leader>sg", require("telescope.builtin").live_grep, { desc = "[S]earch by [G]rep" })
-- vim.keymap.set("n", "<leader>sG", ":LiveGrepGitRoot<cr>", { desc = "[S]earch by [G]rep on Git Root" })
-- vim.keymap.set("n", "<leader>sd", require("telescope.builtin").diagnostics, { desc = "[S]earch [D]iagnostics" })
-- vim.keymap.set("n", "<leader>sr", require("telescope.builtin").resume, { desc = "[S]earch [R]esume" })
-- TODO: fix probably a telescope function we can use.
-- vim.keymap.set('n', '<leader>pa', ':args `rg --files --hidden --ignore-vcs -g !{**/node_modules/*,**/.git/*}`<cr>')
vim.keymap.set("n", "<leader>pg", ":grep  <cr>")
--}}}
--Lists {{{
vim.keymap.set("n", "]q", ":cnext<cr>")
vim.keymap.set("n", "[q", " :cprev<cr>", { remap = false })
vim.keymap.set("n", "]wq", ":cwindow<cr>")
vim.keymap.set("n", "]wl", ":lwindow<cr>")
vim.keymap.set("n", "]l", ":lnext<cr>")
vim.keymap.set("n", "[l", ":lprev<cr>")
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Go to next diagnostic message" })
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Go to previous diagnostic message" })

--}}}
--Jumps {{{
vim.keymap.set("n", "<leader>j=", 'gg=G<C-o>:echo "Indented buffer"<cr>')
vim.keymap.set("n", "<leader>jp", "<C-o>")
vim.keymap.set("n", "<leader>jn", "<C-i>")
--}}}
--Buffers {{{
vim.keymap.set("n", "<leader>bb", ":Telescope buffers<cr>")
--Switch to the last buffer and delete this one.
vim.keymap.set("n", "<leader>bd", ":bprevious|bdelete #<cr>")
vim.keymap.set("n", "<leader>bs", ":SC<cr>")
vim.keymap.set("n", "<leader>br", ":e %<cr>")
--}}}
--Windows {{{
vim.keymap.set("n", "<leader>w", "<C-w>")
vim.keymap.set("n", "<M-o>", "<C-w>w")
--}}}
--Terminal {{{
vim.keymap.set("n", "<leader>'", ":terminal<cr>")
vim.keymap.set("t", "fd", "<C-\\><C-n>", { remap = false })
vim.keymap.set("t", "<C-o>", "<C-\\><C-n>")
vim.keymap.set("n", "<leader>1", ":!<cr>")
vim.keymap.set("n", "<leader>t'", ":tabnew<cr>:terminal<cr>")
--}}}
--Tabs {{{
vim.keymap.set("n", "]t", ":tabnext<cr>")
vim.keymap.set("n", "[t", ":tabprev<cr>")
vim.keymap.set("t", "<M-j>", "<C-\\><C-n>:tabnext", { remap = false })
vim.keymap.set("t", "<M-k>", "<C-\\><C-n>:tabprev", { remap = false })
vim.keymap.set("n", "<leader>tn", ":tabnext<cr>")
vim.keymap.set("n", "<leader>tp", ":tabprev<cr>")
vim.keymap.set("n", "<leader>to", ":tabnew<cr>")
vim.keymap.set("n", "<leader>tc", ":tabclose<cr>")
vim.keymap.set("n", "<leader>tw", "<C-w>T")
vim.keymap.set("n", "<leader>t1", ":tabn 1<cr>")
vim.keymap.set("n", "<leader>t2", ":tabn 2<cr>")
vim.keymap.set("n", "<leader>t3", ":tabn 3<cr>")
vim.keymap.set("n", "<leader>t4", ":tabn 4<cr>")
vim.keymap.set("n", "<leader>t5", ":tabn 5<cr>")
--}}}
--Git {{{
vim.keymap.set("n", "<leader>gg", ":Git ")
vim.keymap.set("n", "<leader>gs", ":Git<cr>")
vim.keymap.set("n", "<leader>gl", ":Git log<cr>")
vim.keymap.set("n", "<leader>gp", ":Git push<cr>")
vim.keymap.set("n", "<leader>gP", ":Git pull --rebase<cr>")
vim.keymap.set("n", "<leader>gri", ":Git rebase -i origin/master<cr>")
vim.keymap.set("n", "<leader>gb", ":Git blame<cr>")
vim.keymap.set("n", "<leader>gv", ":Git vader<cr>")
--}}}
--Utility {{{
--Copy file name to clipboard
vim.keymap.set("n", ",cs", ':let @*=expand("%")<CR>')
vim.keymap.set("n", ",cl", ':let @*=expand("%:p")<CR>')
--On MacOS I tell my keyboard to use alt as meta as the Lord intended but that
--means I can't type the hash symbol on my UK keyboards. This fixes that.
if vim.fn.has("macunix") then
	vim.keymap.set("i", "<M-3>", "#")
end
--Easily run commands
vim.keymap.set("n", "!", ":!")
--}}}
--System / Vim management {{{
vim.keymap.set("n", "<leader>spu", ":Lazy update<cr>")
--}}}
--Testing {{{
vim.keymap.set("n", "<leader>mt", ":RunTest<CR>")
--}}}
-- LSP {{{
-- Diagnostic keymaps
-- vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, { desc = "Open floating diagnostic message" })
-- vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostics list" })
-- }}}
