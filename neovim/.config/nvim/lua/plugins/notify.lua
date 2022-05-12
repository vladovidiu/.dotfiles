local notify_status_ok, notify = pcall(require, "notify")
if not notify_status_ok then
	return
end

vim.notify = notify
