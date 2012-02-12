nnoremap <F11>   :if strlen(expand("%")) <Bar> write <Bar> endif <CR> :w !ass
nnoremap <S-F11> :if strlen(expand("%")) <Bar> write <Bar> endif <CR> :r !gen
nnoremap <C-F11> :let current_header = expand("%")<CR><C-W><C-N> :exec "normal! i#include \"" . current_header . "\"\n"<CR>
