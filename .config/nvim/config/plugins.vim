let g:airline_powerline_fonts = 1 " Pretty symbols from airline
" let g:airline#extensions#tabline#enabled = 1  " Show a cool tabline to help me see my buffers

call plug#begin('~/.vim-plugged')
""" Fuzzy finding
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

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
Plug 'tpope/vim-obsession' " Better session management

""" Language Support 
Plug 'PProvost/vim-ps1'     " I write powershell scripts
Plug 'leafgarland/typescript-vim'
Plug 'Vimjas/vim-python-pep8-indent' " better python indentation

""" Editor improvements
Plug 'jiangmiao/auto-pairs'      " Auto closing of pairs
Plug 'alvan/vim-closetag'        " Close (X)HTML tags
Plug 'easymotion/vim-easymotion' " Easily jump around files

""" Linting
Plug 'w0rp/ale' " Code linting and LSP client
"
""" Auto completion
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

""" IDE-esque support for various languages
Plug 'sbdchd/neoformat'              " Format various sources which have a supported formatter
Plug 'fatih/vim-go', { 'for': 'go' } " Better go support
Plug 'pangloss/vim-javascript'       " Better Javascript

""" Themes
Plug 'icymind/NeoSolarized', { 'as': 'solarized' }

""" Easy alignment
Plug 'junegunn/vim-easy-align'

""" Writing
Plug 'junegunn/goyo.vim'      " Distraction free writing like writeroom-mode
call plug#end()

let g:go_fmt_autosave = 1
let g:go_fmt_command = "goimports"

let g:deoplete#enable_at_startup = 1

" Force ALE to use language servers when necessary
let g:ale_linters = {}
let g:ale_linters.rust = ['rls']
let g:ale_linters.cpp = ['clangd']
let g:ale_rust_rls_toolchain = 'stable' " this is needed, otherwise rls uses nightly toolchain

" [Tags] Command to generate tags file
let g:fzf_tags_command = '/usr/local/bin/ctags -R'
