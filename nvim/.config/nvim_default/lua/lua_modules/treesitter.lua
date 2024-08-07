local treesitter = require('nvim-treesitter.configs')

treesitter.setup {
    ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "elm", "python" },

    highlight = {
        enable = true
    }
}

local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.org = {
  install_info = {
    url = 'https://github.com/milisims/tree-sitter-org',
    revision = 'main',
    files = { 'src/parser.c', 'src/scanner.cc' },
  },
  filetype = 'org',
}
