if has('macunix')
    let g:python3_host_prog="/usr/local/bin/python3"
endif

let $NVIM_TUI_ENABLE_TRUE_COLOR=1
filetype plugin on
syntax enable
set termguicolors

set expandtab
set smarttab

""" Theme settings
source $HOME/.config/nvim/config/editor.vim
source $HOME/.config/nvim/config/keys.vim
source $HOME/.config/nvim/config/plugins.vim
source $HOME/.config/nvim/config/autocmds.vim
source $HOME/.config/nvim/config/utils.vim
source $HOME/.config/nvim/config/my_statusline.vim
lua require('tools')

""" Load Theme
set background=dark
colorscheme dracula

highlight! LineNr guifg=white guibg='#424450' ctermfg=white ctermbg=238
highlight! SignColumn guifg=white guibg='#424450' ctermfg=white ctermbg=238


highlight! link LspReferenceText DraculaSelection
highlight! link LspReferenceRead DraculaSelection
highlight! link LspReferenceWrite DraculaSelection
