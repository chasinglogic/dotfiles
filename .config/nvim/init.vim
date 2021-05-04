let $NVIM_TUI_ENABLE_TRUE_COLOR=1
set termguicolors

source $HOME/.config/nvim/config/editor.vim
source $HOME/.config/nvim/config/keys.vim
source $HOME/.config/nvim/config/plugins.vim
source $HOME/.config/nvim/config/autocmds.vim
source $HOME/.config/nvim/config/utils.vim
source $HOME/.config/nvim/config/my_statusline.vim
lua require('config')

""" Load Theme
colorscheme Dracula

highlight! LineNr guifg=white guibg='#424450' ctermfg=white ctermbg=238
