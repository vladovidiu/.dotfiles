local async = require("plenary.async")

local map = vim.keymap.set

local packer_sync = function()
	async.run(function()
		vim.notify.async("Syncing packer.", "info", {
			title = "Packer",
		})
	end)
	local snap_shot_time = os.date("!%Y-%m-%dT%TZ")
	vim.cmd("PackerSnapshot " .. snap_shot_time)
	vim.cmd("PackerSync")
end

map("n", "<leader>ps", "", {
	callback = packer_sync,
})

local set_cwdir = function()
	async.run(function()
		vim.notify.async("Setting current directory", "info", {
			title = "Vim",
		})
	end)
	vim.cmd("lcd %:p:h")
end

map("n", "<leader>cd", "", {
	callback = set_cwdir,
})
