" Ultisnips {{{
let g:UltiSnipsExpandTrigger = "<c-j>"
let g:UltiSnipsJumpForwardTrigger	= "<c-j>"
let g:UltiSnipsJumpBackwardTrigger	= "<c-k>"
" }}}
" Plugins {{{
call plug#begin('~/.local/share/vim/plugins')
" External Tool Integration {{{
""" Fuzzy finding
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
" }}}
" Tpope general improvements {{{
Plug 'tpope/vim-vinegar'    " Netrw improvements
Plug 'tpope/vim-commentary' " Commenting code
Plug 'tpope/vim-surround'   " Surrounding of text
Plug 'tpope/vim-sleuth'     " Set tabwidth etc based on filetype
Plug 'tpope/vim-eunuch'     " Useful commands like Rename, Delete, Move, SudoWrite
Plug 'tpope/vim-abolish'    " Better abbreviations and Subvert is like fancy %s
Plug 'tpope/vim-fugitive'   " Git integration
Plug 'tpope/vim-rhubarb'    " Github integration
Plug 'tpope/vim-endwise'    " Automatically add 'end' and similar language constructs
Plug 'tpope/vim-rsi'        " Readline bindings in the vim command line
" }}}
" Language Support {{{
Plug 'sheerun/vim-polyglot'
" }}}
" Editor improvements {{{
Plug 'jiangmiao/auto-pairs'      " Auto pair things
Plug 'alvan/vim-closetag'        " Close (X)HTML tags
Plug 'AndrewRadev/splitjoin.vim' " Easily split single-line statements to multi-line
Plug 'SirVer/UltiSnips'          " Snippets in vim
Plug 'junegunn/vim-easy-align'   " Align stuff.
" }}}
" IDE-like Features (Linting, Formatting, completion etc.) {{{
Plug 'sbdchd/neoformat' " Format various sources which have a supported formatter
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-lua/completion-nvim'
Plug 'steelsojka/completion-buffers'

Plug 'dense-analysis/ale'    " Code linting
" }}}
" Themes {{{
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'overcache/NeoSolarized'
" }}}
call plug#end()
" }}}
" Neoformat {{{
let g:neoformat_enabled_python = ['black', 'isort', 'docformatter']
let g:neoformat_enabled_python3 = ['black',  'isort', 'docformatter']
" }}}
" Split-join {{{
let g:splitjoin_ruby_trailing_comma = 1
let g:splitjoin_python_brackets_on_separate_lines = 1
" }}}
" LSP {{{
lua require('settings.telescope')
lua require('settings.lsp')

"" Use <Tab> and <S-Tab> to navigate through popup menu
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Set completeopt to have a better completion experience
set completeopt=menuone,noinsert,noselect

" Avoid showing message extra message when using completion
set shortmess+=c
autocmd BufEnter * lua require'completion'.on_attach()

lua require('settings.completion')
" }}}
" Auto Pairs {{{
inoremap <buffer> <silent> <CR> <C-R>=AutoPairsReturn()<CR>
" }}}
