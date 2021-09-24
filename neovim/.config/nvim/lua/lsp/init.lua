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

