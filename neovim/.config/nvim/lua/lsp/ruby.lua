local lspconfig = require('lspconfig')

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

local on_attach = function(client, bufnr)
  local function buf_set_keymap(...)
    vim.api.nvim_buf_set_keymap(bufnr, ...)
  end

  local opts = {silent = true}

  buf_set_keymap('n', 'gs', ':TSLspOrganize<CR>', opts)
  buf_set_keymap('n', 'qq', ':TSLspFixCurrent<CR>', opts)
  buf_set_keymap('n', 'gr', ':TSLspRenameFile<CR>', opts)
  buf_set_keymap('n', 'gi', ':TSLspImportAll<CR>', opts)

  vim.cmd("autocmd BufWritePost <buffer> lua vim.lsp.buf.formatting()")
  vim.opt_local.omnifunc = "v:lua.vim.lsp.omnifunc"
end

lspconfig.solargraph.setup {
  on_attach = on_attach,
  capabilities = capabilities,
  settings = {solargraph = {diagnostics = true, completion = true}}
}
