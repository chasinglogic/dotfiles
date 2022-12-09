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
Plug 'nvim-telescope/telescope.nvim', { 'rev': '0.1.x' }
Plug 'nvim-telescope/telescope-fzy-native.nvim'  " Stricly a speed improvement for Telescope
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
Plug 'martinda/Jenkinsfile-vim-syntax'
Plug 'towolf/vim-helm'
" }}}
" Editor improvements {{{
Plug 'gbprod/yanky.nvim'                   " Emacs-like kill ring.
Plug 'SirVer/UltiSnips'                    " Snippets in vim
Plug 'quangnguyen30192/cmp-nvim-ultisnips' " Autocompletion for snippets
" }}}
" IDE-like Features (Linting, Formatting, completion etc.) {{{
Plug 'sbdchd/neoformat' " Format various sources which have a supported formatter
Plug 'windwp/nvim-autopairs'  " Auto pair things
Plug 'windwp/nvim-ts-autotag' " Auto close and rename JSX/html tags
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/cmp-nvim-lsp'   " Autocomplete LSP suggestions
Plug 'hrsh7th/cmp-buffer'     " Autocomplete based on words in buffer
Plug 'hrsh7th/cmp-path'       " Autocomplete filepaths
Plug 'hrsh7th/nvim-cmp'
Plug 'gabrielpoca/replacer.nvim' " Allows you to edit the quickfix window
" }}}
" Themes {{{
Plug 'ishan9299/nvim-solarized-lua'
Plug 'dracula/vim', { 'as': 'dracula' }
" }}}
call plug#end()
" }}}
" Neoformat {{{
let g:neoformat_enabled_python = ['black', 'isort', 'docformatter']
let g:neoformat_enabled_python3 = ['black',  'isort', 'docformatter']
let g:neoformat_try_formatprg = 1
" }}}
" LSP & Completion {{{

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
