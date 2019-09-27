let g:airline_powerline_fonts = 1 " Pretty symbols from airline
" let g:airline#extensions#tabline#enabled = 1  " Show a cool tabline to help me see my buffers

call plug#begin('~/.vim-plugged')
""" Fuzzy finding
Plug 'wincent/command-t', {
    \   'do': 'cd ruby/command-t/ext/command-t && ruby extconf.rb && make'
    \ }

" Plug 'Shougo/denite.nvim'

Plug 'gabrielelana/vim-markdown'
Plug 'jamessan/vim-gnupg'

""" Better folding for text formats like Markdown
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

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

""" Language Support
Plug 'PProvost/vim-ps1'              " I write powershell scripts
Plug 'leafgarland/typescript-vim'    " Add typescript syntax files
Plug 'Vimjas/vim-python-pep8-indent' " better python indentation

""" Editor improvements
Plug 'jiangmiao/auto-pairs'      " Auto closing of pairs
Plug 'alvan/vim-closetag'        " Close (X)HTML tags
Plug 'easymotion/vim-easymotion' " Easily jump around files

""" Linting
" Plug 'dense-analysis/ale' " Code linting and LSP client

""" Auto completion
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'autozimu/LanguageClient-neovim', {
      \ 'branch': 'next',
      \ 'do': 'bash install.sh',
      \ }

""" IDE-esque support for various languages
Plug 'sbdchd/neoformat' " Format various sources which have a supported formatter
Plug 'fatih/vim-go', { 'for': 'go' }
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }

""" Themes
Plug 'icymind/NeoSolarized', { 'as': 'solarized' }

""" Easy alignment
Plug 'junegunn/vim-easy-align'

""" Writing
Plug 'junegunn/goyo.vim'      " Distraction free writing like writeroom-mode
call plug#end()

""" Language client configuration
let g:deoplete#enable_at_startup = 1
let g:LanguageClient_useVirtualText = 0
let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'stable', 'rls'],
    \ 'javascript': ['javascript-typescript-stdio'],
    \ 'javascript.jsx': ['tcp://127.0.0.1:2089'],
    \ 'python': ['pyls'],
    \ 'c': ['ccls'],
    \ 'cpp': ['ccls'],
    \ }

nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>

""" Vim-go settings
let g:go_fmt_autosave = 1
let g:go_fmt_command = "goimports"

" [Tags] Command to generate tags file
let g:fzf_tags_command = '/usr/local/bin/ctags -R'
