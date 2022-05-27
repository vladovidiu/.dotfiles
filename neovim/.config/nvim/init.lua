local present, impatient = pcall(require, "impatient")

if present then
	impatient.enable_profile()
end

require("plugins-bootstrap")
require("settings")
require("keybindings")
require("lsp")
require("plugins")
require("functions")
