local lualine_theme = require'lualine.themes.solarized_light'

require'lualine'.setup{
  options = { theme  = lualine_theme },
}

-- lualine shows the mode so hide the default vim mode
vim.o.showmode = false
