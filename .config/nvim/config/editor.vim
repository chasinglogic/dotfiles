" Tab size {{{
set tabstop=4     " A tab is 4 spaces
set expandtab     " Always uses spaces instead of tabs
set softtabstop=4 " Insert 4 spaces when tab is pressed
set shiftwidth=4  " An indent is 4 spaces
set shiftround    " Round indent to nearest shiftwidth multiple
" }}}
" Formatting options {{{
set textwidth=80       " 80 Column text width
set formatoptions-=ro  " Auto insert comment character when making a new line
set formatoptions+=q   " Allow formatting of comments with gq
set formatoptions+=n   " recognize numbered lists
set formatoptions+=j   " Remove comment char when joining lines
set formatoptions-=t   " No visual wrapping
set nowrap             " Don't do soft wrapping
set nostartofline      " Don't go to the start of line after certain commands
set autoindent         " Copy indent from current line when starting a new line
filetype plugin indent on
" }}}
" Tab completion settings for command line {{{
set wildignore+=*.o,*.git,*.svn,*.pyc "ignore these files
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
" Ripgrep with vimgrep {{{
if executable("rg")
    set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case
elseif executable("ag")
    set grepprg=ag\ --nogroup\ --nocolor\ --hidden
else
    set grepprg=grep\ -R
endif
" }}}
" Use system clipboard {{{
set clipboard+=unnamedplus
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
set undofile                 " keep persistent undo across vim runs
set undodir=$HOME/.local/share/vim/undo/ " where to store undo files
if !isdirectory(&undodir)
    call mkdir(&undodir, "p")
endif
" }}}
" Completion {{{
set completeopt=noinsert,menuone,noselect
" }}}
" Auto read files from disk when changed {{{
set autoread
au CursorHold * checktime
" }}}
" Open big split for file with V in netrw {{{
let g:netrw_altv=1
" }}}
" Tagfiles {{{
set tags=.tags,tags
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
" From ncm2 suggested settings
set shortmess+=c
" }}}
