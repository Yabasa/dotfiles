" general -------------------------------------------------------------------{{{

    " open .vimrc in a split
    nnoremap <leader>ev :split $MYVIMRC<cr>

    " source my .vimrc
    nnoremap <leader>sv :source $MYVIMRC<cr>

    " escape insert more without having to reach to escape key
    inoremap jk <esc>

    " search always starts with \v so that very magic mode is always used for regex
    nnoremap / /\v
    nnoremap ? ?\v

    " Must be defined early in config before vim-perl is loaded
    let perl_fold=1

" }}}


" pulgins -------------------------------------------------------------------{{{

    call plug#begin('~/.config/nvim/plugged')

    " Color scheme
    Plug 'overcache/NeoSolarized'

    " Dependency for a lot of lua plugins
    Plug 'nvim-lua/plenary.nvim'

    " Telescope
    Plug 'nvim-telescope/telescope.nvim'
    Plug 'nvim-telescope/telescope-fzf-native.nvim', {'do': 'make' }

    " Icons
    Plug 'kyazdani42/nvim-web-devicons'

    " Status line
    Plug 'hoob3rt/lualine.nvim'

    " LSP
    Plug 'neovim/nvim-lspconfig'

    " Snippets
    Plug 'L3MON4D3/LuaSnip'

    " Auto complete
    Plug 'hrsh7th/nvim-cmp'
    Plug 'hrsh7th/cmp-nvim-lsp'
    Plug 'saadparwaiz1/cmp_luasnip'
    Plug 'onsails/lspkind-nvim'

    " Treesitter
    Plug 'nvim-treesitter/nvim-treesitter', { 'do': ':TSUpdate' }

    " Useful stuff
    Plug 'lewis6991/gitsigns.nvim'
    Plug 'tpope/vim-surround'
    Plug 'windwp/nvim-autopairs'
    Plug 'voldikss/vim-floaterm'
    Plug 'tpope/vim-projectionist'
    Plug 'numToStr/Comment.nvim'


    call plug#end()

    lua require('lua_modules')

" }}}


" movement ------------------------------------------------------------------{{{

    " begining and end of line are capital H and L (hard left and hard right)
    nnoremap H ^
    nnoremap L $

" }}}


" general editing behaviour -------------------------------------------------{{{

    " move a line down
    nnoremap - ddp

    " convert word to uppercase
    inoremap <c-u> <esc>viwUwi

    " tabs as spaces
    set tabstop=4       " tabs are 4 spaces wide
    set shiftwidth=4    " indents are 4 spaces wide
    set expandtab       " insert spaces when tab is pressed

    " auto indent
    set autoindent
    set smartindent

" }}}


" view ----------------------------------------------------------------------{{{

    set background=dark       " set background to dark theme
    colorscheme NeoSolarized  " set the color scheme

    set number                " show the absolute line number
    set relativenumber        " show the relative line numbers

    set foldlevelstart=0      " close all folds on open
    set foldmethod=expr
    set foldexpr=nvim_treesitter#foldexpr()

    " toggle fold
    nnoremap <space> za

    set hlsearch              " highlight search matches
    set incsearch             " dynamically highlight match while typing

    set ignorecase            " case insensitive search
    set smartcase             " make search case sensitive if capital letters are used

    " turn off highlight from last search
    nnoremap <leader>ho :nohlsearch<cr>

    " set focus to the opened split
    set splitbelow
    set splitright

    " Have only one status line at the bottom
    set laststatus=3

    " Floating terminal
    nnoremap <leader>t :FloatermToggle<cr>
    tnoremap <leader>q <C-\><C-n>:q<cr>

" }}}


" file type setup ----------------------------------------------------------{{{

    " vimscript {{{

        augroup filetype_vim
            autocmd!

            " comment and uncomment lines
            autocmd FileType vim nnoremap <buffer> <localleader>c mqI"<esc>`q
            autocmd FileType vim nnoremap <buffer> <localleader>u mq:s/^\(\s*\)"\=/\1<cr>`q

            " folding
            autocmd FileType vim setlocal foldmethod=marker
            autocmd FileType vim setlocal foldlevel=1
        augroup END

    " }}}

   " perl {{{

        augroup filetype_perl
            autocmd!

            " comment and uncomment lines
            autocmd FileType perl nnoremap <buffer> <localleader>c mqI#<esc>`q
            autocmd FileType perl nnoremap <buffer> <localleader>u mq:s/^\(\s*\)#\=/\1<cr>`q

            " folding
            autocmd FileType perl setlocal foldlevel=1

        augroup END

    "}}}

   " git {{{

        augroup filetype_git
            autocmd!
            autocmd FileType gitcommit set tw=72
        augroup END

    "}}}

" }}}
