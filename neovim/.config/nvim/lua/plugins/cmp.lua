local cmp = require("cmp")
local lspkind = require("lspkind")
local luasnip = require("luasnip")

local check_backspace = function()
	local col = vim.fn.col(".") - 1
	return col == 0 or vim.fn.getline("."):sub(col, col):match("%s")
end

cmp.setup({
	sources = {
		{ name = "nvim_lua" },
		{ name = "nvim_lsp" },
		{ name = "path" },
		{ name = "vsnip" },
		{ name = "treesitter" },
		{ name = "buffer" },
	},
	mapping = {
		["<C-d>"] = cmp.mapping.scroll_docs(-4),
		["<C-f>"] = cmp.mapping.scroll_docs(4),
		["<C-Space>"] = cmp.mapping.complete(),
		["<C-e>"] = cmp.mapping.close(),
		["<CR>"] = cmp.mapping.confirm({ select = true }),
		["<Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			elseif luasnip.expandable() then
				luasnip.expand()
			elseif check_backspace() then
				fallback()
			else
				fallback()
			end
		end, {
			"i",
			"s",
		}),
		["<S-Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_prev_item()
			elseif luasnip.expandable() then
				luasnip.expand()
			elseif check_backspace() then
				fallback()
			else
				fallback()
			end
		end, {
			"i",
			"s",
		}),
	},
	snippet = {
		expand = function(args)
			require("luasnip").lsp_expand(args.body)
		end,
	},
	experimental = { native_menu = false, ghost_text = true },
	formatting = {
		format = lspkind.cmp_format({
			with_text = true,
			menu = {
				buffer = "[buf]",
				nvim_lsp = "[LSP]",
				nvim_lua = "[api]",
				path = "[path]",
				luasnip = "[snip]",
			},
		}),
	},
	sorting = {
		comparators = {
			cmp.config.compare.exact,
			cmp.config.compare.locality,
			cmp.config.compare.recently_used,
			cmp.config.compare.score,
			cmp.config.compare.offset,
			cmp.config.compare.sort_text,
			cmp.config.compare.order,
		},
	},
	confirm_opts = {
		behavior = cmp.ConfirmBehavior.Replace,
		select = false,
	},
	window = {
		completion = cmp.config.window.bordered({
			winhighlight = "NormalFloat:NormalFloat,FloatBorder:FloatBorder",
		}),
		documentation = cmp.config.window.bordered({
			winhighlight = "NormalFloat:NormalFloat,FloatBorder:FloatBorder",
		}),
	},
})
