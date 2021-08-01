require'nvim-treesitter.configs'.setup {
  ensure_installed = 'maintained',
  highlight = {enable = true},
  incremental_selection = {enable = true},
  indent = {enable = true},
  rainbow = {
    enable = true,
    extended_mode = true, -- Highlight also non-parentheses delimiters, boolean or table: lang -> boolean
    max_file_lines = 5000 -- Do not enable for files with more than 1000 lines, int
  }
}

