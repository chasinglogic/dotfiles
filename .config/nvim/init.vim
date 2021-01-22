if has('macunix')
    let g:python3_host_prog="/usr/local/bin/python3"
endif

let $NVIM_TUI_ENABLE_TRUE_COLOR=1
filetype plugin on
syntax enable
set termguicolors

""" Theme settings
source $HOME/.config/nvim/config/editor.vim
source $HOME/.config/nvim/config/keys.vim
source $HOME/.config/nvim/config/plugins.vim
source $HOME/.config/nvim/config/autocmds.vim
source $HOME/.config/nvim/config/utils.vim
source $HOME/.config/nvim/config/my_statusline.vim
source $HOME/.config/nvim/config/abbrevs.vim

""" Load Theme
augroup CustomColors
    autocmd!
    autocmd ColorScheme * highlight LineNr guifg=white guibg='#424450' ctermfg=white ctermbg=238
augroup END

let g:neosolarized_contrast = "high"
set background=dark
colorscheme NeoSolarized
