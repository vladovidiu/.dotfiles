local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  execute(
      '!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
  execute 'packadd packer.nvim'
end

return require('packer').startup(function()
  use {'wbthomason/packer.nvim'}

  use {
    'eddyekofo94/gruvbox-flat.nvim',
    config = function()
      vim.g.gruvbox_italic_functions = true
      vim.g.gruvbox_sidebars = {'packer', 'terminal'}
      vim.g.gruvbox_transparent = true
      vim.cmd [[
        colorscheme gruvbox-flat
      ]]
    end
  }

  use {'hrsh7th/cmp-nvim-lsp'}
  use {'hrsh7th/cmp-buffer'}
  use {'hrsh7th/nvim-cmp'}

  use {'hrsh7th/cmp-vsnip'}
  use {'hrsh7th/vim-vsnip'}

  use {'neovim/nvim-lspconfig'}
  use {'glepnir/lspsaga.nvim'}

  use {
    'nvim-treesitter/nvim-treesitter',
    requires = {'p00f/nvim-ts-rainbow'},
    run = ':TSUpdate'
  }

  use {'nvim-treesitter/playground'}

  use {'svermeulen/vimpeccable'}

  use {
    'nvim-telescope/telescope.nvim',
    requires = {
      {'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'},
      {'kyazdani42/nvim-web-devicons'}
    }
  }

  use {
    'jose-elias-alvarez/nvim-lsp-ts-utils',
    requires = {'jose-elias-alvarez/null-ls.nvim'}
  }

  use {'crispgm/nvim-go'}
  use {'sebdah/vim-delve'}

  use {'onsails/lspkind-nvim'}

  use {'simrat39/rust-tools.nvim'}

  use {
    'hoob3rt/lualine.nvim',
    requires = {'kyazdani42/nvim-web-devicons', opt = true}
  }

end)
