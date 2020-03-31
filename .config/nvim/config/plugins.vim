let g:airline_powerline_fonts = 1 " Pretty symbols from airline

""" Vimwiki
let g:vimwiki_map_prefix = '<Leader>k'
let g:vimwiki_list = [{'path': '~/Dropbox/Notes',
                      \ 'syntax': 'markdown', 'ext': '.md'}]

call plug#begin('~/.vim-plugged')
""" Fuzzy finding
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

""" Automatically encrypt and decrypt my gpg notes
Plug 'jamessan/vim-gnupg'

""" Prettier and more functional statusline
Plug 'itchyny/lightline.vim'

""" Tpope general improvements
Plug 'tpope/vim-vinegar'    " Netrw improvements
Plug 'tpope/vim-commentary' " Commenting code
Plug 'tpope/vim-surround'   " Surrounding of text
Plug 'tpope/vim-endwise'    " Add 'end' in ruby and bash scripts smartly
Plug 'tpope/vim-sleuth'     " Set tabwidth etc based on filetype
Plug 'tpope/vim-eunuch'     " Useful commands like Rename, Delete, Move, SudoWrite
Plug 'tpope/vim-abolish'    " Better abbreviations and Subvert is like fancy %s
Plug 'tpope/vim-fugitive'   " Git integration
Plug 'tpope/vim-markdown'   " Better markdown
Plug 'tpope/vim-obsession'  " Automatic session management and restoration

""" Language Support
Plug 'PProvost/vim-ps1'              " I write powershell scripts sometimes
Plug 'leafgarland/typescript-vim'    " Add typescript syntax files
Plug 'Vimjas/vim-python-pep8-indent' " better python indentation
Plug 'igankevich/mesonic'
Plug 'arrufat/vala.vim'
Plug 'vim-pandoc/vim-pandoc'         " Better folding for text formats like Markdown
Plug 'vim-pandoc/vim-pandoc-syntax'

""" Editor improvements
Plug 'jiangmiao/auto-pairs'      " Auto closing of pairs
Plug 'alvan/vim-closetag'        " Close (X)HTML tags
Plug 'AndrewRadev/splitjoin.vim' " Easily split single-line statements to multi-line

""" Linting

""" Auto completion
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'dense-analysis/ale'    " Code linting and LSP client

""" IDE-esque support for various languages
Plug 'sbdchd/neoformat' " Format various sources which have a supported formatter
Plug 'fatih/vim-go', { 'for': 'go' }
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }

""" Themes
Plug 'icymind/NeoSolarized', { 'as': 'solarized' }
Plug 'tomasr/molokai'
Plug 'chasinglogic/modus-themes-vim'

""" Easy alignment
Plug 'junegunn/vim-easy-align'

""" Writing
Plug 'junegunn/goyo.vim'      " Distraction free writing like writeroom-mode

""" Wiki for notes
Plug 'vimwiki/vimwiki'

call plug#end()

""" Neoformat
let g:neoformat_enabled_python = ['black', 'docformatter']

let g:vala_syntax_folding_enabled = 0

""" Language client configuration
let g:deoplete#enable_at_startup = 1
call deoplete#custom#option('sources', {'_': [ 'ale', 'buffer'], 'python': ['ale', 'buffer']})

nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>

let g:neoformat_enabled_python = ['black']
let g:neoformat_enabled_python3 = ['black']

let g:lightline = {
      \ 'colorscheme': 'solarized',
      \ }

""" Vim-go settings
let g:go_fmt_autosave = 1
let g:go_fmt_command = "goimports"

""" Split-join
let g:splitjoin_ruby_trailing_comma = 1
let g:splitjoin_python_brackets_on_separate_lines = 1

" [Tags] Command to generate tags file
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

" Use a nice floating window for display

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
