" open .vimrc in a split
nnoremap <leader>ev :split $MYVIMRC<cr>

" source my .vimrc
nnoremap <leader>sv :source $MYVIMRC<cr>

" escape insert more without having to reach to escape key
inoremap <esc> <nop>
inoremap jk <esc>

" search always starts with \v so that very magic mode is always used for regex
nnoremap / /\v
nnoremap ? ?\v

execute pathogen#infect()


" pulgins ------------------------------------------------------------------{{{
    
    " Pathogen make installing other plugins much easier

" }}}


" view --------------------------------------------------------------------{{{
    
    let g:solarized_termtrans=1 " needed to set this for PuTTY
    set background=dark         " set background to dark theme 
    colorscheme solarized       " set the color scheme

    set number              " show the absolute line number
    set relativenumber      " show the relative line numbers
    hi LineNr term=NONE     " gets rid of underlining of line numbers
    
    set laststatus=2        " always show the status line
    set statusline=%.60F\ -\ FileType:\ %y%=%l/%L:%c

    set foldlevelstart=0    " close all folds on open

    " toggle fold
    nnoremap <space> za

    set hlsearch            " highlight search matches
    set incsearch           " dynamically highlight match while typing

    " turn off highlight from last search
    nnoremap <leader>ho :nohlsearch<cr>

    " highlight all trailing whitespace with red backgroud
    nnoremap <leader>w :match Error /\v\s+$/<cr>

" }}}


" movement -----------------------------------------------------------------{{{

    " unmap arrow keys in normal mode
    nnoremap <up> <nop>
    nnoremap <down> <nop>
    nnoremap <left> <nop>
    nnoremap <right> <nop>

    " unmap arrow keys in insert mode
    inoremap <up> <nop>
    inoremap <down> <nop>
    inoremap <left> <nop>
    inoremap <right> <nop>

    " begining and end of line are capital H and L (hard left and hard right)
    nnoremap H ^
    nnoremap L $

" }}}


" general editing behaviour --------------------------------------------------------{{{

    " move a line down
    nnoremap - ddp

    " delete line while in insert mode
    inoremap <c-d> <esc>ddi

    " convert word to uppercase
    inoremap <c-u> <esc>viwUwi

    " surround word in double qoutes
    nnoremap <leader>" viw<esc>a"<esc>bi"<esc>lel

    " surround word in single quotes
    nnoremap <leader>' viw<esc>a'<esc>bi'<esc>lel

    " surround a block of visually selected text in double qoutes
    vnoremap <leader>" <esc>`<i"<esc>`>a"<esc>

    " surround a block of visually selected text in single qoutes
    vnoremap <leader>' <esc>`<i'<esc>`>a'<esc>

    " tabs as spaces
    set tabstop=4       " tabs are 4 spaces wide
    set shiftwidth=4    " indents are 4 spaces wide
    set expandtab       " insert spaces when tab is pressed

    " auto indent
    set autoindent

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
        autocmd FileType perl let perl_fold=1
        autocmd FileType perl setlocal foldlevel=1

        syntax on

        " add semicolon
        autocmd FileType perl nnoremap <buffer> <localleader>; mq:s/;*$/;<cr>`q
    augroup END
    "}}}

   " git {{{
    augroup filetype_git
        autocmd!
        autocmd FileType gitcommit set tw=72
    augroup END
    "}}}

" }}}


