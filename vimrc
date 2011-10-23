noremap <F11>   :w !ass  
noremap <S-F11> :let current_header = expand("%")<CR><C-W><C-N> :exec "normal! i#include \"" . current_header . "\"\n"<CR>
