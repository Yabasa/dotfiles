{pkgs, rocPkgs, ...}:
{
  home.username = "treb";
  home.homeDirectory = "/home/treb";
  home.stateVersion = "24.11";
  programs.home-manager.enable = true;
  home.packages = with pkgs; [
    # hello
    rocPkgs.cli
    rocPkgs.lang-server
    exercism
    nodejs
    elmPackages.elm
    elmPackages.lamdera
  ];

  programs.git.enable = true;

  programs.nixvim = {
    enable = true;
    defaultEditor = true;

    viAlias = true;
    vimAlias = true;

    luaLoader.enable = true;

    extraPlugins = [ pkgs.vimPlugins.nvim-solarized-lua ];
    colorscheme = "solarized";

    extraConfigLua = ''
    vim.cmd [[autocmd BufWritePre <buffer> lua vim.lsp.buf.format({ async = false })]]

    -- make .roc files have the correct filetype
    vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter" }, {
      pattern = { "*.roc" },
      command = "set filetype=roc",
    })

    -- add roc tree-sitter
    local parsers = require("nvim-treesitter.parsers").get_parser_configs()

    parsers.roc = {
      install_info = {
        url = "https://github.com/faldor20/tree-sitter-roc",
        files = { "src/parser.c", "src/scanner.c" },
      },
    }
    do
        local __lspServers = { { ["name"] = "roc_ls" } }
        local __lspOnAttach = function(client, bufnr) end
        local __lspCapabilities = function()
            capabilities = vim.lsp.protocol.make_client_capabilities()

            return capabilities
        end

        local __setup = {
            on_attach = __lspOnAttach,
            capabilities = __lspCapabilities(),
        }

        for i, server in ipairs(__lspServers) do
            if type(server) == "string" then
                require("lspconfig")[server].setup(__setup)
            else
                local options = server.extraOptions

                if options == nil then
                    options = __setup
                else
                    options = vim.tbl_extend("keep", options, __setup)
                end

                require("lspconfig")[server.name].setup(options)
            end
        end
    end'';

    plugins = {
      lsp = {
        enable = true;

        keymaps = {
          silent = true;
          diagnostic = {
            # Navigate in diagnostics
            "<leader>k" = "goto_prev";
            "<leader>j" = "goto_next";
          };

          lspBuf = {
            gd = "definition";
            gD = "references";
            gt = "type_definition";
            gi = "implementation";
            K = "hover";
            "<F2>" = "rename";
          };
        };

        servers = {
          lua_ls.enable = true;
          nil_ls.enable = true;
          elmls.enable = true;
        };
      };
    };

    plugins.lualine = {
      enable = true;
      settings.options.theme = "solarized";
    };
    plugins.gitsigns.enable = true;
    plugins.web-devicons.enable = true;
    plugins.telescope = {
    	enable = true;
        extensions.fzf-native.enable = true;
        keymaps = {
            "<leader>ff" = "find_files";
            "<leader>fg" = "live_grep";
            "<leader>fb" = "buffers";
            "<leader>fh" = "help_tags";
        };
    };
    plugins.treesitter.enable = true;

    opts = {
      # Line numbers
      relativenumber = true; # Relative line numbers
      number = true; # Display the absolute line number of the current line
      splitbelow = true; # A new window is put below the current one
      splitright = true; # A new window is put right of the current one
      ignorecase = true; # When the search query is lower-case, match both lower and upper-case
      #   patterns
      smartcase = true; # Override the 'ignorecase' option if the search pattern contains upper
      #   case characters
      scrolloff = 8; # Number of screen lines to show around the cursor
      laststatus = 3; # When to use a status line for the last window

      # Tab options
      tabstop = 4; # Number of spaces a <Tab> in the text stands for (local to buffer)
      shiftwidth = 4; # Number of spaces used for each step of (auto)indent (local to buffer)
      expandtab = true; # Expand <Tab> to spaces in Insert mode (local to buffer)
      autoindent = true; # Do clever autoindenting
      hlsearch = true;
      incsearch = true;
      showmode = false;
      background = "light";
    };

    keymaps = [
      {
        action = "<esc>";
        key = "jk";
	mode = "i";
      }
      {
        action = "^";
        key = "H";
	mode = "n";
      }
      {
        action = "$";
        key = "L";
	mode = "n";
      }
      {
        action = "za";
        key = "<space>";
	mode = "n";
      }
      {
        action = ":nohlsearch<cr>";
        key = "<leader>ho";
	mode = "n";
      }
    ];
  };
}
