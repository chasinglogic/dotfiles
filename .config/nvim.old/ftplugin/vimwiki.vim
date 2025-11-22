nmap - :Explore<CR>

augroup AutoGenIndex
    autocmd BufWritePost *.md silent !~/.local/bin/generate_vimwiki_index
augroup END
