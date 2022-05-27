local set_keymap = vim.keymap.set

local options = { silent = true }

vim.g.mapleader = " "
set_keymap("n", "<Space>", "")

set_keymap("n", "<leader><esc>", "<cmd>set nohls<cr>", options)

-- Telescope
-- Find files using Telescope command-line sugar.
set_keymap("n", "<leader>pf", "<cmd>Telescope find_files<CR>", options)
set_keymap("n", "<leader>sp", "<cmd>Telescope live_grep<CR>", options)
set_keymap("n", "<leader>bb", "<cmd>Telescope buffers<CR>", options)
set_keymap("n", "<leader>fh", "<cmd>Telescope help_tags<CR>", options)

-- Nvim LSP
set_keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", options)
set_keymap("n", "<leader>gd", "<cmd>lua vim.lsp.buf.type_definition<CR>", options)
set_keymap("n", "gs", "<cmd>lua vim.lsp.buf.signature_help()<CR>", options)
set_keymap("n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", options)
set_keymap("n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", options)
set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", options)
set_keymap("n", "<C-k>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", options)
set_keymap("n", "<leader>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", options)
set_keymap("n", "<leader>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>", options)
set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", options)
set_keymap("n", "[d", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>", options)
set_keymap("n", "]d", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>", options)
set_keymap("n", "<leader>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", options)

-- neogit
set_keymap("n", "<leader>gg", "<cmd>Neogit<CR>", options)
