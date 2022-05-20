-- Color for highlights
local colors = { cyan = "#8ec07c", magenta = "#d3869b" }

local config = {
	options = {
		lower = true,
		icons_enabled = true,
		theme = "gruvbox",
		padding = 1,
		component_separators = { "", "" },
		section_separators = { "", "" },
		disabled_filetypes = {},
	},
	sections = {
		lualine_a = { "mode" },
		lualine_b = { "branch", "diff" },
		lualine_c = { "filename" },
		lualine_x = { "fileformat", "filetype" },
		lualine_y = { "progress" },
		lualine_z = { "location" },
	},
	inactive_sections = {
		lualine_a = {},
		lualine_b = {},
		lualine_c = { "filename" },
		lualine_x = { "location" },
		lualine_y = {},
		lualine_z = {},
	},
	tabline = {},
	extensions = {},
}

local function ins_left(component)
	table.insert(config.sections.lualine_c, component)
end

local function ins_right(component)
	table.insert(config.sections.lualine_x, component)
end

ins_left({
	"lsp_progress",
	colors = {
		percentage = colors.cyan,
		title = colors.cyan,
		message = colors.cyan,
		spinner = colors.cyan,
		lsp_client_name = colors.magenta,
		use = true,
	},
	separators = {
		component = " ",
		progress = " | ",
		message = { pre = "(", post = ")" },
		percentage = { pre = "", post = "%% " },
		title = { pre = "", post = ": " },
		lsp_client_name = { pre = "[", post = "]" },
		spinner = { pre = "", post = "" },
	},
	-- add spinner if needed
	display_components = { "lsp_client_name", { "title", "percentage", "message" } },
	timer = {
		progress_enddelay = 500,
		spinner = 1000,
		lsp_client_name_enddelay = 2000,
	},
	spinner_symbols = {
		"🌑 ",
		"🌒 ",
		"🌓 ",
		"🌔 ",
		"🌕 ",
		"🌖 ",
		"🌗 ",
		"🌘 ",
	},
})

require("lualine").setup(config)
