local lspconfig = require('lspconfig')

local on_attach = function(client, bufnr)
  client.resolved_capabilities.document_formatting = false

  local function buf_set_keymap(...)
    vim.api.nvim_buf_set_keymap(bufnr, ...)
  end

  local opts = {silent = true}
  buf_set_keymap('n', '<leader>f', ':lua vim.lsp.buf.formatting()', opts)

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
