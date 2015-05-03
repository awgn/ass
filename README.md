Ass++
-----

ASSi is an interactive C++11 code assistant inspired to GHCi. 

It can be used through vim or interactively as a shell command. It supports both gcc and clang compilers, libstdc++, libc++ and few boost headers (like the geordi bot). 
It features precompiled headers and tab completion for commands, file names and C++ identifiers. 

C++ statements are evaluated on the fly, possibly using the source code loaded, no matter if it is provided with the main function or if it is just a class declaration.
Namespaces are deducted parsing the source code and members declared therein are accessible through automatic using-namespace declarations. 

In addition, ASSi provides some C++ goodies that allow to test code quickly. It provides an oracle class O that can be injected into containers, and few utility functions. 
S() stringifies showable expressions (STL containers, tuples, smart pointers, chrono and streamable types), type names can be demangled with T<type>()/type_of() and ranges ala Haskell 
are available through the R function which generates a suitable std::initializer_list.


Help
----

    usage: ass [OPTION] [COMPILER OPT] -- [ARG]
        -i              launch interactive mode
        -l  file        launch interactive mode + load file
        -v, --version   show version
        -h, --help      print this help


Session
-------

    ASSi, version 2.23 :? for help
    Compilers found: g++-4.8 g++-4.7 g++-4.6 clang-34 
    Using Gcc48 compiler...
    
    Ass Gcc48> :?
    Commands available from the prompt:

    <statement>                 evaluate/run C++ <statement>
      :include file             add include in the buffer
      :load file                load file in the buffer
      :reload                   reload the file
      :edit                     edit the buffer
      :list                     list the buffer
      :clear                    clear the buffer
      :next                     switch to next compiler
      :args ARG1 ARG2...        set runtime arguments
      :run [ARG1 ARG2...]       run the main function
      :rr                       reload and run the main function
      :info TYPE                show info about the given TYPE
      :preload                  toggle preload std headers
      :verbose                  show additional information
      :quit                     quit
      :?                        print this help

    C++ goodies:
      _s _h,_min,_s,_ms,_us...  string and chrono user-defined literals
      _(1,2,3)                  tuple/pair constructor
      P(arg1, arg2, ...)        variadic print
      T<type>()                 demangle the name of a type
      type_of(v)                deduce the type of a given expression
      R(1,2,5)                  range: initializer_list<int> {1,2,3,4,5}
      S(v),SHOW(v)              stringify a value
      hex(v), oct(v), bin(v)    show manipulators
      class O                   oracle class.

    Using Gcc48 compiler...
    Ass>
    Ass> vector<int> v = R(1,2,10); return v;
    [ 1 2 3 4 5 6 7 8 9 10 ] [ExitSuccess]
    Ass> 
    Ass> return _("hello world", 42);
    ("hello world",42) [ExitSuccess]
    Ass> 
    Ass> auto x = O{}; auto y (std::move(x)); return y;
    O() O(O&&) ~O() O@0x7fffad138640 ~O() [ExitSuccess]
    Ass> 
    Ass> return system_clock::now();
    1388498574873117227_ns [ExitSuccess]
    Ass>
    Ass> :load test.cpp 
    loading test.cpp...
    Ass> :list 
    #include <iostream>
    #include <vector>
    
    template <typename T>
    using test = std::vector<T>;
    
    int
    main(int argc, char *argv[])
    {
        std::cout << "argc = " << argc << std::endl;
        return 0;
    }
    
    Ass> :run 1 2
    argc = 3
    [ExitSuccess]
    Ass> auto x = test<int>{1,2,3}; return x;
    [ 1 2 3 ] [ExitSuccess]
    Ass>

    Ass> :info int
    type name: int
    is_const: false
    is_volatile: false
    is_trivial: true
    is_standard_layout: true
    is_pod: true
    is_literal_type: true
    is_empty: false
    is_polymorphic: false
    is_abstract: false
    is_signed: true
    is_unsigned: false
    is_constructible: true
    is_nothrow_constructible: true
    is_trivially_copyable: true
    is_trivially_constructible: true
    is_trivially_default_constructible: true
    is_trivially_copy_constructible: true
    is_trivially_move_constructible: true
    is_trivially_copy_assignable: true
    is_trivially_move_assignable: true
    is_default_constructible: true
    is_nothrow_default_constructible: true
    is_copy_constructible: true
    is_nothrow_copy_constructible: true
    is_move_constructible: true
    is_nothrow_move_constructible: true
    is_copy_assignable: true
    is_nothrow_copy_assignable: true
    is_nothrow_move_assignable: true
    is_destructible: true
    is_move_assignable: true
    is_trivially_destructible: true
    is_nothrow_destructible: true
    has_virtual_destructor: false
    sizeof   : 4
    alignment: 4
    default  : {????}
    uniform  : {0000}
    value    : {0000}

    Ass> :quit
    Leaving ASSi. 
