local status_ok, impatient = pcall(require, "impatient")
if status_ok then
	impatient.enable_profile()
end

require("vt.defaults")
require("vt.keymaps")

require("vt.plugins")
require("vt.utils")

require("vt.lsp")
