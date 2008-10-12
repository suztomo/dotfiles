set tabstop=2
set background=dark

let g:treeExplVertical=1
set iminsert=1

set shiftwidth=1

function! AutoUp()
    if expand('%') =~ g:svbfre && !&readonly && &buftype == ''
        silent update
    endif
endfunction

autocmd CursorHold * call AutoUp()
set updatetime=50
let g:svbfre = '.\+'

set noswapfile
set nobackup
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplmapCTabSwitchBufs = 1
let g:miniBufExplModSelTarget = 1

set incsearch
set hlsearch
set fdm=marker
nnoremap i <ESC>:noh<CR>i
nnoremap <space>w :w<CR>
"autocmd BufRead * noh
syntax on


