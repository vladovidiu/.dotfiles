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
