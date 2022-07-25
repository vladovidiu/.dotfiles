local mason_status_ok, mason = pcall(require, "mason")
if not mason_status_ok then
	return
end

local mason_lspconfig_status_ok, mason_lspconfig = pcall(require, "mason-lspconfig")
if not mason_lspconfig_status_ok then
	return
end

local go_status_ok, go = pcall(require, "go")
if not go_status_ok then
	return
end

local rust_status_ok, rust_tools = pcall(require, "rust-tools")
if not rust_status_ok then
	return
end

local lspconfig = require("lspconfig")
local servers = {
	"jsonls",
	"gopls",
	"sumneko_lua",
	"tsserver",
	"rust_analyzer",
	"taplo",
	"sumneko_lua",
}

go.setup({
	test_runner = "richgo",
	run_in_floaterm = true,
})

mason.setup({
	ui = {
		icons = {
			package_installed = "✓",
			package_pending = "➜",
			package_uninstalled = "✗",
		},
		border = "rounded",
	},
})

mason_lspconfig.setup({
	ensure_installed = servers,
})

-- Register a handler that will be called for all installed servers.
-- Alternatively, you may also register handlers on specific server instances instead (see example below).
for _, server in pairs(servers) do
	local opts = {
		on_attach = require("lsp.handlers").on_attach,
		capabilities = require("lsp.handlers").capabilities,
	}

	local has_custom_opts, server_custom_opts = pcall(require, "lsp.settings." .. server)

	if has_custom_opts then
		opts = vim.tbl_deep_extend("force", server_custom_opts, opts)
	end

	if server == "rust_analyzer" then
		local rust_opts = require("lsp.settings.rust")

		rust_tools.setup(rust_opts)
		goto continue
	end

	lspconfig[server].setup(opts)
	::continue::
end
