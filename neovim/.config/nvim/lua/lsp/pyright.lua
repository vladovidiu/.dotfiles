local lspconfig = require("lspconfig")

lspconfig.pyright.setup {
  on_attach = function(_, bufnr)
    local function buf_set_keymap(...)
      vim.api.nvim_buf_set_keymap(bufnr, ...)
    end

    local opts = {silent = true}

    buf_set_keymap("n", "ct", ":!pytest<CR>", opts)
  end
}
