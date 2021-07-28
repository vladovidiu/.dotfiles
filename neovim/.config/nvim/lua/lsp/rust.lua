local lspconfig = require('lspconfig')

local on_attach = function()
  vim.cmd("autocmd BufWritePost <buffer> lua vim.lsp.buf.formatting()")
end

lspconfig.rust_analyzer.setup {
  on_attach = on_attach,
  settings = {
    ["rust-analyzer"] = {
      cargo = {loadOutDirsFromCheck = true},
      procMacro = {enable = true}
    }
  }
}

require("rust-tools").setup {on_attach = on_attach}
