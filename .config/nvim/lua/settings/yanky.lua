require("yanky").setup({
    ring = {

        -- Define the storage mode for ring values.
        --
        -- Using shada, this will save pesistantly using Neovim ShaDa feature.
        -- This means that history will be persisted between each session of
        -- Neovim.
        --
        -- You can also use this feature to sync the yank history across
        -- multiple running instances of Neovim by updating shada file. If you
        -- execute :wshada in the first instance and then :rshada in the second
        -- instance, the second instance will be synced with the yank history in
        -- the first instance.
        --
        -- Using memory, each Neovim instance will have his own history and il
        -- will be lost between sessions.
        --
        -- Sqlite is more reliable than ShaDa but requires more dependencies.
        storage = "memory",
    },
    -- Not sure how I feel about these yet.
    highlight = {
        on_put = true,
        on_yank = true,
        timer = 100
    },
    preserve_cursor_position = {
      enabled = true,
    },
})


vim.keymap.set({"n","x"}, "y", "<Plug>(YankyYank)")
vim.keymap.set({"n","x"}, "p", "<Plug>(YankyPutAfter)")
vim.keymap.set({"n","x"}, "P", "<Plug>(YankyPutBefore)")
vim.keymap.set({"n","x"}, "gp", "<Plug>(YankyGPutAfter)")
vim.keymap.set({"n","x"}, "gP", "<Plug>(YankyGPutBefore)")
vim.keymap.set("n", "<c-n>", "<Plug>(YankyCycleForward)")
vim.keymap.set("n", "<c-p>", "<Plug>(YankyCycleBackward)")

-- Load the telescope integration.
require("telescope").load_extension("yank_history")
vim.keymap.set({"n"}, "<Leader>pp", ":Telescope yank_history<CR>")
