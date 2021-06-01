" Ultisnips {{{
let g:UltiSnipsExpandTrigger = "<c-j>"
let g:UltiSnipsJumpForwardTrigger	= "<c-j>"
let g:UltiSnipsJumpBackwardTrigger	= "<c-k>"
" }}}

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
Plug 'tjdevries/nlua.nvim'
Plug 'hashivim/vim-terraform'
" }}}
" Editor improvements {{{
Plug 'SirVer/UltiSnips'          " Snippets in vim
" }}}
" IDE-like Features (Linting, Formatting, completion etc.) {{{
Plug 'sbdchd/neoformat' " Format various sources which have a supported formatter
Plug 'steelsojka/pears.nvim'     " Auto pair things
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/nvim-compe'
" }}}
" Themes {{{
Plug 'ishan9299/nvim-solarized-lua'
Plug 'sainnhe/sonokai'
" }}}
call plug#end()
" }}}
" Neoformat {{{
let g:neoformat_enabled_python = ['black', 'isort', 'docformatter']
let g:neoformat_enabled_python3 = ['black',  'isort', 'docformatter']
let g:neoformat_try_formatprg = 1
" }}}
" LSP & Completion {{{

" Use <Tab> and <S-Tab> to navigate through popup menu
inoremap <silent><expr> <C-Space> compe#complete()
inoremap <silent><expr> <CR>      compe#confirm('<CR>')

" Set completeopt to have a better completion experience
set completeopt=menuone,noselect

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
