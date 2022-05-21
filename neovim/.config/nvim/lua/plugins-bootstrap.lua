local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
	PACKER_BOOTSTRAP = fn.system({
		"git",
		"clone",
		"--depth",
		"1",
		"https://github.com/wbthomason/packer.nvim",
		install_path,
	})
	print("Installing packer close and reopen Neovim...")

	vim.cmd([[packadd packer.nvim]])
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins-bootstrap.lua source <afile> | PackerSync
  augroup end
]])

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
	return
end

-- Have packer use a popup window
packer.init({
	display = {
		open_fn = function()
			return require("packer.util").float({ border = "rounded" })
		end,
	},
})

return packer.startup(function(use)
	use({ "wbthomason/packer.nvim" })
	use("nvim-lua/popup.nvim") -- An implementation of the Popup API from vim in Neovim
	use("nvim-lua/plenary.nvim") -- Useful lua functions used ny lots of plugins

	use({
		"gruvbox-community/gruvbox",
		config = function()
			vim.cmd([[
        let g:gruvbox_contrast_dark = 'hard'
        let g:gruvbox_invert_selection='0'
        colorscheme gruvbox

        highlight ColorColumn ctermbg=0 guibg=grey
        hi SignColumn guibg=none
        hi CursorLineNR guibg=None
        highlight Normal guibg=none
        highlight NormalFloat guibg=none
        highlight FloatBorder guibg=#5eacd

        " highlight LineNr guifg=#ff8659
        " highlight LineNr guifg=#aed75f
        highlight LineNr guifg=#5eacd3
        highlight netrwDir guifg=#5eacd3
        highlight qfFileName guifg=#aed75f
        hi TelescopeBorder guifg=#5eacd
      ]])
		end,
	})

	use({ "L3MON4D3/LuaSnip", requires = { "rafamadriz/friendly-snippets" } })

	-- cmp plugins
	use("hrsh7th/nvim-cmp") -- The completion plugin
	use("hrsh7th/cmp-buffer") -- buffer completions
	use("hrsh7th/cmp-path") -- path completions
	use("hrsh7th/cmp-nvim-lua")
	use("hrsh7th/cmp-nvim-lsp")
	use("hrsh7th/cmp-cmdline") -- cmdline completions
	use("ray-x/cmp-treesitter") -- cmdline completions
	use("saadparwaiz1/cmp_luasnip") -- snippet completions

	use({ "RishabhRD/popfix" })
	use({ "RishabhRD/nvim-lsputils" })

	-- LSP
	use("neovim/nvim-lspconfig") -- enable LSP
	use("williamboman/nvim-lsp-installer") -- simple to use language server installer
	use("tamago324/nlsp-settings.nvim") -- language server settings defined in json for
	use("jose-elias-alvarez/null-ls.nvim") -- for formatters and linters

	use({
		"nvim-telescope/telescope.nvim",
		requires = {
			{ "nvim-lua/popup.nvim" },
			{ "nvim-lua/plenary.nvim" },
			{ "kyazdani42/nvim-web-devicons" },
		},
	})

	use({ "onsails/lspkind-nvim" })

	-- Treesitter
	use({
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
	})
	use("p00f/nvim-ts-rainbow")
	use("nvim-treesitter/playground")
	use("JoosepAlviste/nvim-ts-context-commentstring")

	-- Notify
	use("rcarriga/nvim-notify")

	-- go.nvim
	use("ray-x/go.nvim")

	-- Float term, codeaction, codelens gui
	use({ "ray-x/guihua.lua", run = "cd lua/fzy && make" })

	-- Autopairs
	use("windwp/nvim-autopairs")

	-- rust
	use("simrat39/rust-tools.nvim")

	-- feline
	use("feline-nvim/feline.nvim")

	-- GPS
	use("SmiteshP/nvim-gps")

	-- gitsigns
	use("lewis6991/gitsigns.nvim")

	-- neogit
	use("TimUntersberger/neogit")

	-- Colorizer
	use("norcalli/nvim-colorizer.lua")
end)
