local cmp = require('cmp')
local lspkind = require('lspkind')

cmp.setup({
  sources = {
    {name = 'nvim_lua'}, {name = 'nvim_lsp', priority = 50}, {name = 'path'},
    {name = 'vsnip'}, {name = 'buffer'}
  },
  mapping = {
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm({select = true})
  },
  snippet = {
    expand = function(args)
      require('luasnip').lsp_expand(args.body)
    end
  },
  experimental = {native_menu = false, ghost_text = true},
  formatting = {
    format = lspkind.cmp_format {
      with_text = true,
      menu = {
        buffer = '[buf]',
        nvim_lsp = '[LSP]',
        nvim_lua = '[api]',
        path = '[path]',
        luasnip = '[snip]'
      }
    }
  }
})

