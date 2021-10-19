require('lsp.tsserver')
require('lsp.sumneko_lua')
require('lsp.efm')
require('lsp.nvim_go')
require('lsp.pyright')
require('lsp.rust')
require('lsp.ccls')
require('lsp.clangd')
require('lsp.cmake')
require('lsp.ruby')

vim.fn.sign_define("LspDiagnosticsSignError",
                   {text = "", texthl = "GruvboxRed"})
vim.fn.sign_define("LspDiagnosticsSignWarning",
                   {text = "", texthl = "GruvboxYellow"})
vim.fn.sign_define("LspDiagnosticsSignInformation",
                   {text = "", texthl = "GruvboxBlue"})
vim.fn.sign_define("LspDiagnosticsSignHint",
                   {text = "", texthl = "GruvboxAqua"})

vim.lsp.handlers['textDocument/codeAction'] =
    require'lsputil.codeAction'.code_action_handler

vim.lsp.handlers['textDocument/references'] =
    require'lsputil.locations'.references_handler

vim.lsp.handlers['textDocument/definition'] =
    require'lsputil.locations'.definition_handler

vim.lsp.handlers['textDocument/declaration'] =
    require'lsputil.locations'.declaration_handler

vim.lsp.handlers['textDocument/typeDefinition'] =
    require'lsputil.locations'.typeDefinition_handler

vim.lsp.handlers['textDocument/implementation'] =
    require'lsputil.locations'.implementation_handler

vim.lsp.handlers['textDocument/documentSymbol'] =
    require'lsputil.symbols'.document_handler

vim.lsp.handlers['workspace/symbol'] =
    require'lsputil.symbols'.workspace_handler
