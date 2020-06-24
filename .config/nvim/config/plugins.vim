"" Ultisnips {{{
let g:UltiSnipsExpandTrigger = "<c-j>"
let g:UltiSnipsJumpForwardTrigger	= "<c-j>"
let g:UltiSnipsJumpBackwardTrigger	= "<c-k>"
" }}}
" Polyglot {{{
let g:polyglot_disabled = ['jsx']
" }}}
" Plugins {{{
call plug#begin('~/.local/share/vim/plugins')
" External Tool Integration {{{
""" Fuzzy finding
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
""" Automatically encrypt and decrypt my gpg notes
Plug 'jamessan/vim-gnupg'
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
Plug 'SirVer/ultisnips'          " Snippets in vim
Plug 'junegunn/vim-easy-align'         " Align stuff.
" }}}
" IDE-like Features (Linting, Formatting, completion etc.) {{{
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'deoplete-plugins/deoplete-jedi'
Plug 'deoplete-plugins/deoplete-go', { 'do': 'make'}
Plug 'dense-analysis/ale'    " Code linting and LSP client
Plug 'sbdchd/neoformat' " Format various sources which have a supported formatter
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'sgur/vim-editorconfig'  " Use .editorconfig if present has to be in plugin list after vim-sleuth
Plug 'vim-test/vim-test'
" }}}
" Themes {{{
Plug 'overcache/NeoSolarized', { 'as': 'solarized' }
Plug 'chasinglogic/modus-themes-vim'
" }}}
" Vim for Prose (Blogs, Notes, etc.) {{{
Plug 'junegunn/goyo.vim'      " Distraction free writing like writeroom-mode
" }}}
call plug#end()
" }}}
" Vala {{{
let g:vala_syntax_folding_enabled = 0
" }}}
" Neoformat {{{
let g:neoformat_enabled_python = ['black', 'docformatter']
let g:neoformat_enabled_python3 = ['black', 'docformatter']
" }}}
" Deoplete (Completion) {{{
let g:deoplete#enable_at_startup = 1
" }}}
" Split-join {{{
let g:splitjoin_ruby_trailing_comma = 1
let g:splitjoin_python_brackets_on_separate_lines = 1
" }}}
" FZF {{{
let g:fzf_tags_command = '/usr/local/bin/ctags -R'
" Interactive searchign with Ripgrep
" Taken from: https://github.com/junegunn/fzf.vim#example-advanced-ripgrep-integration
function! RipgrepFzf(query, fullscreen)
  let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case %s || true'
  let initial_command = printf(command_fmt, shellescape(a:query))
  let reload_command = printf(command_fmt, '{q}')
  let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
  call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), a:fullscreen)
endfunction

command! -nargs=* -bang RG call RipgrepFzf(<q-args>, <bang>0)

" }}}
" CoC {{{
let g:coc_global_extensions = ['coc-tsserver', 'coc-python']
" }}}
" Vim Test {{{
let test#strategy = 'neovim'
" }}}
" LanguageClient-neovim {{{
" Required for operations modifying multiple buffers like rename.
set hidden
let g:LanguageClient_serverCommands = {
    \ 'rust': ['~/.cargo/bin/rustup', 'run', 'stable', 'rls'],
    \ 'python': ['/usr/local/bin/pyls'],
    \ 'go': ['~/Code/go/bin/gopls'],
    \ }
nnoremap <F5> :call LanguageClient_contextMenu()<CR>
" }}}
