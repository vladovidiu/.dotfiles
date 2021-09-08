local lspconfig = require('lspconfig')

local on_attach = function()
  vim.cmd("autocmd BufWritePost <buffer> lua vim.lsp.buf.formatting()")
end

lspconfig.clangd.setup {on_attach = on_attach}
