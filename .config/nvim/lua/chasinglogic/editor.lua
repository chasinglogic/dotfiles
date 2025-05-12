--Tab size {{{
vim.opt.tabstop = 4       --  A tab is 4 spaces
vim.opt.smarttab = true   --  Expand the tab to match indentation of line before
vim.opt.expandtab = true  --  Always uses spaces instead of tabs
vim.opt.softtabstop = 4   --  Insert 4 spaces when tab is pressed
vim.opt.shiftwidth = 4    --  An indent is 4 spaces
vim.opt.shiftround = true --  Round indent to nearest shiftwidth multiple
--}}}
--Formatting options {{{
vim.opt.textwidth = 80 --  80 Column text width by default
-- fo-q Allow formatting of comments with gq
-- fo-n Recognize numbered lists
-- fo-j Remove comment char when joining lines
vim.opt.formatoptions:append({
    "q",
    "n",
})
-- Don't know why this can't be in the list above but it bombs out if it is.
vim.opt.formatoptions:append({ "j" })
vim.opt.formatoptions:remove({ "t" }) -- No visual wrapping
vim.opt.wrap = false                  -- Don't do soft wrapping
vim.opt.startofline = false           -- Don't go to the start of line after certain commands
vim.opt.smartindent = true            -- Copy indent from current line when starting a new line
vim.opt.breakindent = true            -- preserving horizontal blocks of text.
--}}}
--Tab completion settings for command line {{{
--Ignore these kinds of files when pressing <TAB> to complete a command.
vim.opt.wildignore:append({ "*.o", "*.git", "*.svn", "*.pyc", "env/*", ".git/*" })
--}}}
--Line numbers {{{
vim.opt.number = true
--}}}
--Enable mouse support {{{
vim.opt.mouse = "a"
--}}}
--Setup an appropriate vimgrep command {{{
if vim.fn.executable("rg") then
    vim.opt.grepprg = "rg --vimgrep --no-heading --smart-case"
else
    vim.opt.grepprg = "grep -R"
end
--}}}
--Use system clipboard {{{
vim.opt.clipboard:append({ "unnamedplus" })
--}}}
--Let find search from pwd {{{
vim.opt.path = "$PWD/**"
--}}}
--Live preview some commands {{{
vim.opt.inccommand = "split"
--}}}
--Intuitive backspacing in insert mode {{{
vim.opt.backspace = { "indent", "eol", "start" }
--}}}
--Backup / Swap {{{
--Backup and swap directories. Deliberate double slash: it makes the backup/swap
--take into account the full path
local backupdir = vim.fn.expand("$HOME/.local/share/vim/backup//")
local swapdir = vim.fn.expand("$HOME/.local/share/vim/swap//")
local undodir = vim.fn.expand("$HOME/.local/share/vim/undo/")
vim.opt.backupdir = backupdir
vim.opt.directory = swapdir
vim.opt.undofile = true   --  keep persistent undo across vim runs
vim.opt.undodir = undodir -- where to store undo files

local ensure_dir = function(dir)
    if not vim.fn.isdirectory(dir) then
        vim.fn.mkdir(dir, "p")
    end
end

ensure_dir(undodir)
ensure_dir(backupdir)
ensure_dir(swapdir)

--}}}
--Completion {{{
-- menu	    Use a popup menu to show the possible completions.  The
--           menu is only shown when there is more than one match and
--           sufficient colors are available.  |ins-completion-menu|

-- menuone  Use the popup menu also when there is only one match.
--          Useful when there is additional information about the
--          match, e.g., what file it comes from.

-- noinsert  Do not insert any text for a match until the user selects
--           a match from the menu. Only works in combination with
--           "menu" or "menuone". No effect if "longest" is present.

-- noselect  Do not select a match in the menu, force the user to
--           select one from the menu. Only works in combination with
--           "menu" or "menuone".
vim.opt.completeopt = { "menuone", "noselect" }
--}}}
--Always utf-8 {{{
vim.opt.encoding = "utf-8"
--}}}
--Case insensitive search by default, unless Capital letter included {{{
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.hlsearch = false --  Don't highlight search matches after search is completed
--}}}
--Open location and quickfix list when running one of those commands {{{
vim.cmd([[
augroup AutoOpenListWindow
    autocmd!
    autocmd QuickFixCmdPost [^l]* cwindow
    autocmd QuickFixCmdPost l*    lwindow
augroup END
]])
--}}}
--Ask to save when trying to close unsaved buffers {{{
vim.opt.confirm = true
--}}}
--Message control {{{
vim.opt.shortmess = vim.opt.shortmess + "c"
--}}}
-- Colour support {{{
vim.o.termguicolors = true
--}}}
-- FileType detection {{{
vim.filetype.add({
    extension = {
        tf = "terraform"
    }
})
-- }}}
