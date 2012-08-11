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
    let g:ass_snippet_file = "\\/tmp\\/snippet.cpp"
endif


" compile snippet and run:
"
function! s:ass_compile_and_run(comp)
        call inputsave() 
        let cmd = inputdialog("[" . a:comp . " ass]: ")
        let fn = expand("%")
        call inputrestore()  
        if strlen(fn) 
            write 
        endif
        %y+ | new | norm P
        if (a:comp == 'gcc')
            exec "silent %! ass " . cmd
        else
            exec "silent %! ass-clang " . cmd
        endif
        exec "silent! %s/" . g:ass_snippet_file . "/" . fn . "/g"
        cgetbuffer | bdelete! | copen
endfunction


" insert c/c++ gate guard:
"
function! s:ass_insert_guard()
        let gatename = "_" . substitute(toupper(expand("%:t")), "[\\.-]", "_", "g") . "_"
        exec "norm! ggI#ifndef " . gatename
        exec "norm! o#define " . gatename . " "
        exec "norm! Go#endif /* " . gatename . " */"
        norm! k
endfunction


" call code generator: 
"
function! s:ass_code_gen()
        call inputsave()
        let cmd =  inputdialog("[gen]: ")
        call inputrestore()
        exec "r !gen " . cmd
endfunction
                       

" include this header in new window:
"
function! s:ass_include_this()
        let current_header = expand("%")
        if !strlen(current_header)
            echoe "[ass]: file with no name cannot be included"
            return 0
        endif
        new | exec "norm! i#include \"" . current_header . "\"\n"
endfunction


command!  AssGccRun         call s:ass_compile_and_run('gcc')
command!  AssClangRun       call s:ass_compile_and_run('clang')
command!  AssGen            call s:ass_code_gen()
command!  AssGuard          call s:ass_insert_guard()
command!  AssIncludeThis    call s:ass_include_this()
