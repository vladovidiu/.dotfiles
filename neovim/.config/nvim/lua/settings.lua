local opt = vim.opt

opt.backup = false
opt.writebackup = false
opt.clipboard = "unnamedplus"
opt.completeopt = { "menu", "menuone", "noselect" }
opt.fileencoding = "utf-8"
opt.ignorecase = true
opt.smartcase = true
opt.pumheight = 20
opt.smartindent = true
opt.splitbelow = true
opt.splitright = true
opt.swapfile = false
opt.termguicolors = true
opt.undofile = true
opt.undodir = vim.fn.stdpath("cache") .. "/undodir"
opt.conceallevel = 0
opt.showmode = false
opt.scrolloff = 5
opt.signcolumn = "yes"
opt.whichwrap = "b,s,<,>,[,],h,l"
opt.laststatus = 3

opt.list = true
opt.listchars:append("tab:»\\ ")
opt.listchars:append("extends:›")
opt.listchars:append("precedes:‹")
opt.listchars:append("nbsp:·")
opt.listchars:append("trail:·")
opt.listchars:append("eol:↲")

vim.wo.wrap = false
vim.wo.number = true
vim.wo.relativenumber = true
vim.wo.cursorline = true

vim.o.colorcolumn = "80"
vim.o.updatetime = 50
vim.o.tabstop = 2
vim.o.softtabstop = 2
vim.o.shiftwidth = 2
vim.o.autoindent = true
vim.bo.copyindent = true
vim.o.expandtab = true

vim.g.loaded_machparen = 1
vim.g.loaded_matchit = 1

vim.g.netrw_banner = 0
vim.g.netrw_silent = 1

vim.cmd([[
  set shortmess+=c
]])

DATA_PATH = vim.fn.stdpath("data")

vim.g.cursorhold_updatetime = 100
