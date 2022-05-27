local escape_status_ok, escape = pcall(require, "better_escape")

if not escape_status_ok then
	return
end

escape.setup({
	mapping = { "jk" },
	timeout = vim.o.timeoutlen,
	clear_empty_lines = false,
	keys = "<Esc>",
})
