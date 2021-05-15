" Ultisnips {{{
let g:UltiSnipsExpandTrigger = "<c-j>"
let g:UltiSnipsJumpForwardTrigger	= "<c-j>"
let g:UltiSnipsJumpBackwardTrigger	= "<c-k>"
" }}}

let g:completion_confirm_key = "<c-l>"

" Plugins {{{
call plug#begin(stdpath('data'))
" Basic Lua Libs {{{
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-lua/popup.nvim'
" }}}
" Fuzzy finding {{{
Plug 'nvim-telescope/telescope.nvim'
" }}}
" Tpope general improvements {{{
Plug 'tpope/vim-abolish'    " Better subst
Plug 'tpope/vim-vinegar'    " Netrw improvements
Plug 'tpope/vim-commentary' " Commenting code
Plug 'tpope/vim-surround'   " Surrounding of text
Plug 'tpope/vim-eunuch'     " Useful commands like Rename, Delete, Move, SudoWrite
Plug 'tpope/vim-fugitive'   " Git integration
Plug 'tpope/vim-endwise'    " Automatically add end or similar constructs
Plug 'tpope/vim-rsi'        " Readline bindings in the vim command line
" }}}
" Language Support {{{
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'Vimjas/vim-python-pep8-indent'
Plug 'tjdevries/nlua.nvim'
Plug 'hashivim/vim-terraform'
Plug 'pangloss/vim-javascript'
" }}}
" Editor improvements {{{
Plug 'steelsojka/pears.nvim'     " Auto pair things
Plug 'SirVer/UltiSnips'          " Snippets in vim
" }}}
" IDE-like Features (Linting, Formatting, completion etc.) {{{
Plug 'sbdchd/neoformat' " Format various sources which have a supported formatter
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-lua/completion-nvim'
Plug 'steelsojka/completion-buffers'
" TODO: Find a way to enable this when LSP doesn't work
Plug 'dense-analysis/ale'    " Code linting

" }}}
" Themes {{{
Plug 'dracula/vim', {'as': 'dracula'}
Plug 'overcache/NeoSolarized'
" }}}
call plug#end()
" }}}
" Neoformat {{{
let g:neoformat_enabled_python = ['black', 'isort', 'docformatter']
let g:neoformat_enabled_python3 = ['black',  'isort', 'docformatter']
let g:neoformat_try_formatprg = 1
" }}}
" LSP {{{
"" Use <Tab> and <S-Tab> to navigate through popup menu
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Set completeopt to have a better completion experience
set completeopt=menuone,noinsert,noselect

" Avoid showing message extra message when using completion
set shortmess+=c
" }}}
" Javascript {{{
let g:javascript_plugin_jsdoc = 1
nnoremap <Leader>rt :JSXReplaceTag<CR>
" }}}
" Terraform {{{
let g:terraform_fmt_on_save=1
" }}}
