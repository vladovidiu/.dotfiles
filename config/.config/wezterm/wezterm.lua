local wezterm = require("wezterm")
local act = wezterm.action
local colors = require("lua/rose-pine").colors()
local window_frame = require("lua/rose-pine").window_frame()

return {
	-- color_scheme = "Gruvbox Dark",
	-- color_scheme = "rose-pine-moon",
	colors = colors,
	max_fps = 200,
	window_frame = window_frame,
	window_background_opacity = 0.90,
	text_background_opacity = 1.0,
	font = wezterm.font("PragmataPro Mono Liga"),
	font_size = 18.0,
	window_decorations = "RESIZE",
	window_padding = {
		left = 2,
		right = 0,
		top = 2,
		bottom = 0,
	},
	hide_tab_bar_if_only_one_tab = true,
	audible_bell = "Disabled",
	cursor_blink_rate = 0,
	visual_bell = {
		fade_in_function = "EaseIn",
		fade_in_duration_ms = 50,
		fade_out_function = "EaseOut",
		fade_out_duration_ms = 50,
	},
	leader = { key = "q", mods = "CTRL", timeout_milliseconds = 1000 },
	keys = {
		{ key = "c", mods = "LEADER", action = act.SpawnTab("CurrentPaneDomain") },
		{ key = "q", mods = "LEADER", action = act.CloseCurrentTab({ confirm = true }) },
		{ key = "n", mods = "LEADER", action = act.ActivateTabRelative(1) },
		{ key = "p", mods = "LEADER", action = act.ActivateTabRelative(-1) },
		{ key = "w", mods = "LEADER", action = act.ShowTabNavigator },

		{ key = "v", mods = "LEADER", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
		{ key = "s", mods = "LEADER", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
		{ key = "x", mods = "LEADER", action = act.CloseCurrentPane({ confirm = true }) },
		{ key = "z", mods = "LEADER", action = act.TogglePaneZoomState },

		{ key = "h", mods = "LEADER", action = act.ActivatePaneDirection("Left") },
		{ key = "l", mods = "LEADER", action = act.ActivatePaneDirection("Right") },
		{ key = "k", mods = "LEADER", action = act.ActivatePaneDirection("Up") },
		{ key = "j", mods = "LEADER", action = act.ActivatePaneDirection("Down") },
		{ key = "H", mods = "LEADER", action = act.AdjustPaneSize({ "Left", 10 }) },
		{ key = "L", mods = "LEADER", action = act.AdjustPaneSize({ "Right", 10 }) },
		{ key = "K", mods = "LEADER", action = act.AdjustPaneSize({ "Up", 5 }) },
		{ key = "J", mods = "LEADER", action = act.AdjustPaneSize({ "Down", 5 }) },

		{ key = "c", mods = "CTRL|SHIFT", action = act.ActivateCopyMode },
		{ key = "v", mods = "CTRL|SHIFT", action = act.PasteFrom("Clipboard") },
	},
}
