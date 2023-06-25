" Tab size {{{
set tabstop=4     " A tab is 4 spaces
set smarttab      " Expand the tab to match indentation of line before
set expandtab     " Always uses spaces instead of tabs
set softtabstop=4 " Insert 4 spaces when tab is pressed
set shiftwidth=4  " An indent is 4 spaces
set shiftround    " Round indent to nearest shiftwidth multiple
" }}}
" Formatting options {{{
set textwidth=80       " 80 Column text width by default
set formatoptions+=q   " Allow formatting of comments with gq
set formatoptions+=n   " recognize numbered lists
set formatoptions+=j   " Remove comment char when joining lines
set formatoptions-=t   " No visual wrapping
set nowrap             " Don't do soft wrapping
set nostartofline      " Don't go to the start of line after certain commands
set smartindent        " Copy indent from current line when starting a new line
filetype plugin indent on
" }}}
" Tab completion settings for command line {{{
" Ignore these kinds of files when pressing <TAB> to complete a command.
set wildignore+=*.o,*.git,*.svn,*.pyc,env/*,.git/*
" }}}
" Line numbers {{{
set number
" }}}
" Enable mouse support {{{
set mouse=a
" }}}
" Keep buffers open in background {{{
set hidden
" }}}
" Setup an appropriate vimgrep command {{{
if executable("rg")
    set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case
elseif executable("ag")
    set grepprg=ag\ --nogroup\ --nocolor\ --hidden
else
    set grepprg=grep\ -R
endif
" }}}
" Use system clipboard {{{

" For whatever reason outside of tmux wl-clipboard and unnamedplus cause
" Neovrim to break gnome-terminal. So just disable the defaul usage of system
" clipboard outside of tmux.
if empty($WAYLAND_DISPLAY) || !empty($TMUX)
    set clipboard+=unnamedplus
endif

" }}}
" Let find search from pwd {{{
set path=$PWD/**
" }}}
" Live preview some commands {{{
set inccommand=split
" }}}
" Intuitive backspacing in insert mode {{{
set backspace=indent,eol,start
" }}}
" Backup / Swap {{{
" Backup and swap directories. Deliberate double slash: it makes the backup/swap
" take into account the full path
set backupdir=$HOME/.local/share/vim/backup//
if !isdirectory(&backupdir)
    call mkdir(&backupdir, "p")
endif

set directory=$HOME/.local/share/vim/swap//
if !isdirectory(&directory)
    call mkdir(&directory, "p")
endif

set undofile " keep persistent undo across vim runs
set undodir=$HOME/.local/share/vim/undo/ " where to store undo files
if !isdirectory(&undodir)
    call mkdir(&undodir, "p")
endif
" }}}
" Completion {{{
"
" menu	    Use a popup menu to show the possible completions.  The
"           menu is only shown when there is more than one match and
"           sufficient colors are available.  |ins-completion-menu|

" menuone  Use the popup menu also when there is only one match.
"          Useful when there is additional information about the
"          match, e.g., what file it comes from.


" noinsert  Do not insert any text for a match until the user selects
"           a match from the menu. Only works in combination with
"           "menu" or "menuone". No effect if "longest" is present.

" noselect  Do not select a match in the menu, force the user to
"           select one from the menu. Only works in combination with
"           "menu" or "menuone".
set completeopt=noinsert,menuone,noselect
" }}}
" Auto read files from disk when changed {{{
set autoread
au CursorHold * checktime
" }}}
" Tagfiles {{{
set tags=TAGS,.tags,tags
" }}}
" Always utf-8 {{{
set encoding=utf-8
" }}}
" Case insensitive search by default, unless Capital letter included {{{
set ignorecase
set smartcase
set nohlsearch " Don't highlight search matches after search is completed
" }}}
" Open location and quickfix list when running one of those commands {{{
augroup AutoOpenListWindow
    autocmd!
    autocmd QuickFixCmdPost [^l]* cwindow
    autocmd QuickFixCmdPost l*    lwindow
augroup END
" }}}
" Ask to save when trying to close unsaved buffers {{{
set confirm
" }}}
" Message control {{{
set shortmess=O
" }}}
" Higlight YankedText after yanking {{{
augroup highlight_yank
    autocmd!
    autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank("IncSearch", 1000)
augroup END
" }}}
