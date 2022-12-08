require('telescope').setup({
    defaults = {
        -- Ripped all these settings from the ivy theme since I can't set
        -- theme = "ivy" as a default.
        --
        -- Found here:
        -- https://github.com/nvim-telescope/telescope.nvim/blob/master/lua/telescope/themes.lua#L109
        sorting_strategy = "ascending",

        layout_strategy = "bottom_pane",
        layout_config = {
          height = 30,
        },

        border = true,
        borderchars = {
          prompt = { "─", " ", " ", " ", "─", "─", " ", " " },
          results = { " " },
          preview = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
        },
    },
    extensions = {
        fzy_native = {
            override_generic_sorter = false,
            override_file_sorter = true,
        }
    },
})

require('telescope').load_extension('fzy_native')
