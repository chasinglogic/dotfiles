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
Plug 'tpope/vim-rhubarb'    " Github integraiton for vim-fugitive
Plug 'tpope/vim-obsession'  " Automatic session management and restoration
Plug 'tpope/vim-endwise'    " Automatically add 'end' and similar language constructs
" }}}
" Language Support {{{
Plug 'leafgarland/typescript-vim'    " Add typescript syntax files
Plug 'Vimjas/vim-python-pep8-indent' " better python indentation
Plug 'igankevich/mesonic'            " Meson.build syntax support
Plug 'arrufat/vala.vim'              " Vala language syntax support
Plug 'vim-pandoc/vim-pandoc'         " Better folding for text formats like Markdown
Plug 'vim-pandoc/vim-pandoc-syntax'
" }}}
" Editor improvements {{{
Plug 'jiangmiao/auto-pairs'      " Auto pair things
Plug 'alvan/vim-closetag'        " Close (X)HTML tags
Plug 'AndrewRadev/splitjoin.vim' " Easily split single-line statements to multi-line
Plug 'SirVer/ultisnips'          " Snippets in vim
Plug 'junegunn/vim-easy-align'   " Automatically align based on regex
Plug 'vim-airline/vim-airline'   " More useful and aesthetic status line
Plug 'vim-airline/vim-airline-themes'
" }}}
" IDE-like Features (Linting, Formatting, completion etc.) {{{
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'deoplete-plugins/deoplete-jedi'
Plug 'dense-analysis/ale'    " Code linting and LSP client
Plug 'sbdchd/neoformat' " Format various sources which have a supported formatter
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'sgur/vim-editorconfig'  " Use .editorconfig if present has to be in plugin list after vim-sleuth
" }}}
" Themes {{{
Plug 'icymind/NeoSolarized', { 'as': 'solarized' }
Plug 'dracula/vim', { 'as': 'dracula' }
" }}}
" Vim for Prose (Blogs, Notes, etc.) {{{
Plug 'junegunn/goyo.vim'      " Distraction free writing like writeroom-mode
" }}}
call plug#end()
" }}}
" Ultisnips {{{
let g:UltiSnipsExpandTrigger		= "<c-x>"
let g:UltiSnipsJumpForwardTrigger	= "<c-j>"
let g:UltiSnipsJumpBackwardTrigger	= "<c-k>"
let g:UltiSnipsRemoveSelectModeMappings = 0
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
" <TAB>: completion.
inoremap <expr> <TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
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

" Neovim 0.4.0 floating window for FZF {{{
if has('nvim-0.4.0')
  let $FZF_DEFAULT_OPTS='--layout=reverse --border'
  let g:fzf_layout = { 'window': 'call FloatingFZF()' }
  let g:fzf_colors =
        \ { 'fg':      ['fg', 'Normal'],
        \   'bg':      ['bg', 'Normal'],
        \   'hl':      ['fg', 'Comment'],
        \   'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
        \   'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
        \   'hl+':     ['fg', 'Statement'],
        \   'info':    ['fg', 'PreProc'],
        \   'border':  ['fg', 'Normal'],
        \   'prompt':  ['fg', 'Conditional'],
        \   'pointer': ['fg', 'Exception'],
        \   'marker':  ['fg', 'Keyword'],
        \   'spinner': ['fg', 'Label'],
        \   'header':  ['fg', 'Comment'] }

  function! FloatingFZF()
    let buf = nvim_create_buf(v:false, v:true)
    let height = float2nr(&lines - (&lines * 2 / 10))
    let width = float2nr(&columns - (&columns * 2 / 10))
    let col = float2nr((&columns - width) / 2)
    let row = float2nr((&lines - height) / 2)

    let opts = {
          \ 'relative': 'editor',
          \ 'row': row,
          \ 'col': col,
          \ 'width': width,
          \ 'height': height,
          \ 'style': 'minimal',
          \ }

    call nvim_open_win(buf, v:true, opts)
  endfunction
endif
" }}}
" }}}
" CoC {{{
let g:coc_global_extensions = ['coc-tsserver', 'coc-python']
" }}}
" Airline {{{
let g:airline_powerline_fonts = 1
let g:airline_theme='solarized'
" }}}
