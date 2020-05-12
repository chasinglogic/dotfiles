let $NVIM_TUI_ENABLE_TRUE_COLOR=1
filetype plugin on
syntax enable
set termguicolors

""" Theme settings
let g:base16colorspace=256
let g:neosolarized_contrast = "high"

source $HOME/.config/nvim/config/editor.vim
source $HOME/.config/nvim/config/keys.vim
if !exists('g:vscode')
    source $HOME/.config/nvim/config/plugins.vim
    source $HOME/.config/nvim/config/autocmds.vim
    source $HOME/.config/nvim/config/utils.vim
    source $HOME/.config/nvim/config/my_statusline.vim
    source $HOME/.config/nvim/config/abbrevs.vim
endif

""" Load Theme
set background=dark
colorscheme NeoSolarized

""" Set font for GUI
if has('gui_running')
    set guifont=Hack:h16
end

set shortmess=O
