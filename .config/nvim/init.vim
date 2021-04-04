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
" Make Dracula more freindly on the eyes
" augroup CustomColors
"     autocmd!
"     autocmd ColorScheme * highlight LineNr guifg=white guibg='#424450' ctermfg=white ctermbg=238
" augroup END

" Default value is "normal", Setting this option to "high" or "low" does use the
" same Solarized palette but simply shifts some values up or down in order to
" expand or compress the tonal range displayed.
let g:neosolarized_contrast = "high"

" I make vertSplitBar a transparent background color. If you like the origin
" solarized vertSplitBar style more, set this value to 0.
let g:neosolarized_vertSplitBgTrans = 1

if luaeval("require('theme_sync').theme_mode()") == "dark"
    set background=dark
else
    set background=light
endif
colorscheme NeoSolarized
