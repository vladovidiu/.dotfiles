local go = require('go')
local lspconfig = require('lspconfig')

go.setup {}
lspconfig.gopls.setup {
  on_attach = function(_, bufnr)
    local function buf_set_keymap(...)
      vim.api.nvim_buf_set_keymap(bufnr, ...)
    end

    local opts = {silent = true}

    buf_set_keymap("n", "ct", ":GoTest<CR>", opts)
    buf_set_keymap("n", "cgt", ":GoToTest<CR>", opts)
    buf_set_keymap("n", "cf", ":Gofmt<CR>", opts)
    buf_set_keymap("n", "cl", ":Golint<CR>", opts)
  end
}
