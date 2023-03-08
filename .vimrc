" the basics
set nocompatible
syntax enable
filetype plugin on

map <space> <leader>

set tabstop=4
set shiftwidth=4
set background=dark

set number
set relativenumber
set smartindent
set encoding=utf-8
set printencoding=utf-8

set showcmd " shows you what you are typing as a command
set hidden  " allow multiple buffers without saving

" sane splits
set splitright
set splitbelow

" use the current working directory and subfolders for searching
set path+=**

" proper search
set incsearch  " highlight while searching
set ignorecase " case insensitive pattern matching
set smartcase  " override ignorecase if pattern contains uppercase
set gdefault   " replace global (///g) as default

" enable autocompletion
set wildmenu
set wildmode=longest,list,full

" tweak netrw (file browser) |netwr-browse-maps|
  "let g:netrw_banner=0
  "TODO: find a good (leader) shortcut for netrw
let g:netrw_browser_split=4 " open in prior window
let g:netrw_altv=1          " open splits to the right
let g:netrw_liststyle=3     " tree view
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

" cursur stuff (WIP)
  "TODO: make the cursor red for better visibility
  "highlight Cursor guifg=white guibg=black
  "highlight iCursor guifg=white guibg=steelblue
  "set guicursor=n-v-c:block-Cursor
  "set guicursor+=i:ver100-iCursor
  "set guicursor+=n-v-c:blinkon0
  "set guicursor+=i:blinkwait10

" overwrite MakeTags command to create ctags
command! MakeTags !ctags -R .

" function to toggle imaps for German
let b:my_german_imaps = "off"

function! ToggleGermanIMaps()
	if b:my_german_imaps == "on"
		iunmap :a
		iunmap :u
		iunmap :o
		iunmap :A
		iunmap :U
		iunmap :O
		iunmap :s
		let b:my_german_imaps = "off"
		echo "German insert mappings disabled"
	else
		imap :a ä
		imap :u ü
		imap :o ö
		imap :A Ä
		imap :U Ü
		imap :O Ö
		imap :s ß
		let b:my_german_imaps = "on"
		echo "German insert mappings enabled"
	endif
endfunction

" disable arrow keys (use home row!)
nnoremap <left>  :echoe "Use h"<CR>
nnoremap <right> :echoe "Use l"<CR>
nnoremap <up>    :echoe "Use k"<CR>
nnoremap <down>  :echoe "Use j"<CR>

" left and right can switch buffers
nnoremap <left>  :bp<CR>
nnoremap <right> :bn<CR>

" move by line (relevant when wrapping)
nnoremap j gj
nnoremap k gk

" map more ESC options
inoremap jk    <esc>
inoremap kj    <esc>
inoremap <C-j> <esc>
inoremap <C-k> <esc>

" ; as : (useful when missing shift)
nnoremap ; :

" jump to start and end of line using the home row keys
map H ^
map L $

" faster split navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" system clipboard integrations
vnoremap <leader>d "+d
vnoremap <leader>y "+y

nnoremap <leader>p "+p
nnoremap <leader>P "+P

" leader shortcuts
map <leader>w :w<CR>
map <leader>W :wa<CR>
map <leader>m :wa<CR>:make<CR>
map <leader>f :find<space>
map <leader>b :b<space>
  "map <leader>g m0:%s/  \+/ /e<CR>gg=G`0

" toggle various things (with <leader>t)
map <leader>ts :set spell!<CR>
map <leader>tn :set number!<CR>:set relativenumber!<CR>
map <leader>tg :call ToggleGermanIMaps()<CR>

if has("autocmd")
	" HTML tag expander
	autocmd FileType html inoremap <C-t> <ESC>"hdiWi<<ESC>"hpa></<ESC>"hpa><ESC>?<<CR>i
	autocmd FileType html nnoremap <C-t> <ESC>"hdiWi<<ESC>"hpa></<ESC>"hpa><ESC>?<<CR>i
	autocmd FileType html inoremap <C-e> <ESC>"hdiWi<<ESC>"hpa></<ESC>"hpa><ESC>?<<CR>i<CR><CR><ESC>ki<TAB>
	autocmd FileType html nnoremap <C-e> <ESC>"hdiWi<<ESC>"hpa></<ESC>"hpa><ESC>?<<CR>i<CR><CR><ESC>ki<TAB>

	" set max width based on the file type
	autocmd BufRead,BufNewFile *.txt setlocal textwidth=80

	" delete trailing whitespace on save
	autocmd BufWritePre * %s/\s\+$//e

	" disable automatic comment insertion
	autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

	" remember cursor position
	autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

	" reload vimrc after saving it
	augroup reload_vimrc
		autocmd!
		autocmd! BufWritePost $MYVIMRC,$MYGVIMRC nested source %
	augroup END
endif
