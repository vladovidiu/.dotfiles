local telescope_status_ok, telescope = pcall(require, "telescope")
if not telescope_status_ok then
	return
end

telescope.setup({
	extensions = {
		fzf = {
			fuzzy = true,
			override_generic_sorter = true,
			override_file_sorter = true,
			case_mode = "smart_case",
		},
	},
	defaults = { file_ignore_patterns = { "node_modules" } },
})

telescope.load_extension("fzf")
telescope.load_extension("packer")
