"" Ultisnips {{{
let g:UltiSnipsExpandTrigger = "<c-j>"
let g:UltiSnipsJumpForwardTrigger	= "<c-j>"
let g:UltiSnipsJumpBackwardTrigger	= "<c-k>"
" }}}
" Plugins {{{
call plug#begin('~/.local/share/vim/plugins')
" External Tool Integration {{{
""" Fuzzy finding
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
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
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
" }}}
" Editor improvements {{{
Plug 'jiangmiao/auto-pairs'      " Auto pair things
Plug 'alvan/vim-closetag'        " Close (X)HTML tags
Plug 'AndrewRadev/splitjoin.vim' " Easily split single-line statements to multi-line
Plug 'SirVer/UltiSnips'          " Snippets in vim
Plug 'junegunn/vim-easy-align'   " Align stuff.
" }}}
" IDE-like Features (Linting, Formatting, completion etc.) {{{
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'deoplete-plugins/deoplete-jedi'
Plug 'dense-analysis/ale'    " Code linting and LSP client
Plug 'sbdchd/neoformat' " Format various sources which have a supported formatter
Plug 'caenrique/nvim-toggle-terminal'
Plug 'davidhalter/jedi-vim'  " Provides go-to-definition for Python
" }}}
" Themes {{{
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'overcache/NeoSolarized'
" }}}
" Vim for Prose (Blogs, Notes, etc.) {{{
Plug 'junegunn/goyo.vim'      " Distraction free writing like writeroom-mode
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
" FZF {{{
function! RipgrepFzf(query, fullscreen)
  let command_fmt = 'rg --hidden --column --line-number --no-heading --color=always --smart-case -- %s || true'
  let initial_command = printf(command_fmt, shellescape(a:query))
  let reload_command = printf(command_fmt, '{q}')
  let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
  call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), a:fullscreen)
endfunction
command! -nargs=* -bang Rg call RipgrepFzf(<q-args>, <bang>0)
" }}}
" Notational Velocity (Notes) {{{
let g:nv_search_paths = ['~/Notes']
" }}}
" Deoplete {{{
let g:deoplete#enable_at_startup = 1
" Use tab for auto complete
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"
" Auto close method doc window when completion done.
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif
" disable autocompletion, because we use deoplete for completion
let g:jedi#completions_enabled = 0
" }}} 
