" ass.vim -- C++ assistant plugin of vim
" @Author:      Nicola
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Revision:    0.1
"

if exists('g:loaded_ass')
    finish
end
let g:loaded_ass = 1


if !exists('g:ass_snippet_file')
    let g:ass_snippet_file = "/tmp/snippet.cpp"
endif


function! s:escape_name(filename)
        return substitute(a:filename, "\/", "\\\\/", "g")
endfunction


" compile snippet and run:
"
function! s:ass_compile_and_run(comp)
        cclose
        call inputsave() 
        let l:cmd = inputdialog("[" . a:comp . " ass]: ")
        call inputrestore()  
        if strlen(expand("%"))
            let l:fname = expand("%")
            write 
        else
            let l:fname = "/tmp/No_name.cpp"
            exec "silent! write! " . l:fname 
        endif
        %y+ | new | normal! P
        if (a:comp == 'gcc')
            exec "silent! %! ass " . l:cmd
        else
            exec "silent! %! ass-clang " . l:cmd
        endif
        exec "silent! %s/" . s:escape_name(g:ass_snippet_file) . "/" . s:escape_name(l:fname) . "/g"
        cgetbuffer | bdelete! | copen
endfunction


" insert c/c++ gate guard:
"
function! s:ass_insert_guard()
        let l:gatename = "_" . substitute(toupper(expand("%:t")), "[\\.-]", "_", "g") . "_"
        exec "normal! ggI#ifndef "  . l:gatename
        exec "normal! o#define "    . l:gatename . " "
        exec "normal! Go#endif /* " . l:gatename . " */"
        normal! k
endfunction


" call code generator: 
"
function! s:ass_code_gen()
        call inputsave()
        let l:cmd =  inputdialog("[gen]: ")
        call inputrestore()
        exec "r !gen " . l:cmd
endfunction
                       

" include this header in new window:
"
function! s:ass_include_this()
        let l:current_header = expand("%")
        if !strlen(l:current_header)
            echoe "[ass]: file with no name cannot be included"
            return 
        endif
        new | exec "normal! i#include \"" . l:current_header . "\"\n"
endfunction


command!  AssGccRun         call s:ass_compile_and_run('gcc')
command!  AssClangRun       call s:ass_compile_and_run('clang')
command!  AssGen            call s:ass_code_gen()
command!  AssGuard          call s:ass_insert_guard()
command!  AssIncludeThis    call s:ass_include_this()
