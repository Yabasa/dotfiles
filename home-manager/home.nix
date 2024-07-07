{pkgs, ...}:
{
  home.username = "treb";
  home.homeDirectory = "/home/treb";
  home.stateVersion = "22.11";
  programs.home-manager.enable = true;
  home.packages = with pkgs; [
    # hello
  ];

  programs.git.enable = true;

  programs.nixvim = {
    enable = true;
    defaultEditor = true;

    viAlias = true;
    vimAlias = true;

    luaLoader.enable = true;

    extraPlugins = [ pkgs.vimPlugins.NeoSolarized ];
    colorscheme = "NeoSolarized";

    extraConfigLua = ''vim.cmd [[autocmd BufWritePre <buffer> lua vim.lsp.buf.format({ async = false })]]'';

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
          lua-ls.enable = true;
          nil-ls.enable = true;
          elmls.enable = true;
        };
      };
    };

    plugins.lualine = {
      enable = true;
      theme = "solarized_light";
    };
    plugins.gitsigns.enable = true;
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
