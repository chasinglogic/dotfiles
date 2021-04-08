local install_script = vim.fn.stdpath('data') 
.. '/nlua.nvim/scripts/download_sumneko.lua'
local c = 'luafile ' .. install_script
vim.cmd(c)
