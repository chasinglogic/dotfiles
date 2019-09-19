""" General Vim Editor configuration

""" Tab size
set tabstop=4     " A tab is 4 spaces
set expandtab     " Always uses spaces instead of tabs
set softtabstop=4 " Insert 4 spaces when tab is pressed
set shiftwidth=4  " An indent is 4 spaces
set shiftround    " Round indent to nearest shiftwidth multiple
set smartindent   " Do intelligent indentation based on programming symbols like {

""" Formatting options
set textwidth=80      " 80 Column text width
set formatoptions+=ro " Auto insert comment character when making a new line
set formatoptions+=q  " Allow formatting of comments with gq
set formatoptions+=n  " recognize numbered lists
set formatoptions+=j  " Remove comment char when joining lines
set formatoptions-=t  " No visual wrapping
set nowrap
set nostartofline     " Don't go to the start of line after certain commands

""" Tab completion settings for command line
set wildignore+=*.o,*.git,*.svn,*.pyc "ignore these files
 
""" Line numbers
set number

""" Enable mouse support
set mouse=a

""" Keep buffers open in background
set hidden

""" Ripgrep with vimgrep
if executable("rg")
    set grepprg=rg\ --vimgrep\ --hidden
else
    set grepprg=grep\ -R
endif

""" Use system clipboard
set clipboard+=unnamedplus

""" Let find search from pwd
set path=$PWD/**

""" Live preview substitution
set inccommand=nosplit

""" Intuitive backspacing in insert mode
set backspace=indent,eol,start

""" Backup / Swap
" Backup and swap directories
" Deliberate double slash: it makes the backup/swap take into account the full
" path
set backupdir=$HOME/.vim-backup//
if !isdirectory(&backupdir)
    call mkdir(&backupdir, "p")
endif

set directory=$HOME/.vim-swap//
if !isdirectory(&directory)
    call mkdir(&directory, "p")
endif

set undofile                 " keep persistent undo across vim runs
set undodir=$HOME/.vim-undo/ " where to store undo files
if !isdirectory(&undodir)
    call mkdir(&undodir, "p")
endif

""" Completion
set completeopt-=preview " Don't show completion preview

""" Auto read files from disk when changed
set autoread                                                                                                                                                                                    
au CursorHold * checktime  

""" Open big split for file with V in netrw
let g:netrw_altv=1

""" Tagfiles
set tags=.tags,tags

set encoding=utf-8

""" Case insensitive search by default, unless Capital letter included 
set ignorecase
set smartcase

""" Open location and quickfix list when running one of those commands 
augroup AutoOpenListWindow
    autocmd!
    autocmd QuickFixCmdPost [^l]* cwindow
    autocmd QuickFixCmdPost l*    lwindow
augroup END

""" Hide grep output when running grep
command! -nargs=* Grep :execute ':silent grep <args>'

""" Ask to save when trying to close unsaved buffers
set confirm

""" Automatically strip trailing whitespace on save
autocmd BufWritePre * %s/\s\+$//e
