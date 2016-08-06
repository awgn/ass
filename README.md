Ass++
-----

ASS++ is an interactive/REPL C++11/14 code assistant inspired to GHCi. 

It can be used through vim or interactively as a command line shell. It supports both gcc and clang compilers, libstdc++, libc++, few boost headers and the functional
cat library (http://cat.github.io). 

It features pre-compiled headers and tab completion for commands, filenames and C++ identifiers. 

C++ statements are evaluated on the fly, possibly using the source code loaded, no matter if it is a complete snippet (with or without main function) or a class declaration.
Namespaces are deduced parsing the source code and members declared therein are accessible through automatic using-namespace declarations. 

In addition, ASS++ provides some C++ goodies that allow to test code quickly. 

It provides an oracle class `O` that can be injected into containers, and few utility functions. 

`S()` stringifies showable expressions (STL containers, tuples, smart pointers, chrono and streamable types); type names can be demangled with `type_name<T>()/type_of()` and ranges ala Haskell 
are available through the `R` function which generates a suitable `std::initializer_list`.

Install
-------

    cabal install --only-dep 
    runhaskell Setup configure --user
    runhaskell Setup build 
    runhaskell Setup install

At the first run don't forget to build pre-compiled headers with:

    ass --build 

Help
----

	Ass++: a REPL C++11/14 assistant

	Usage: ass [-c|--check TARGET] [-l|--load TARGET] [-v|--version]
    	       [-B|--build] [-i|--interactive] [-p|--preload] [-b|--boost]
    	       [-a|--cat] [-- [COMPILER OPTs...] -- [PROG ARGs...]]

	Available options:
	  -h,--help                Show this help text
	  -c,--check TARGET        Check header
	  -l,--load TARGET         Preload module/header
	  -v,--version             Print version
	  -B,--build               (Re)build PHC headers
	  -i,--interactive         Start interactive session
	  -p,--preload             Preload C++ library (auto include)
	  -b,--boost               Preload boost library (PHC)
	  -a,--cat                 Preload cat library (PHC)
	

Session
-------

    ASS++, version 3.0 :? for help
    Compilers found: g++-5 g++-4.9 g++-4.8 g++-4.7 g++-4.6 clang++35 clang++36
    Using Gcc5 compiler...
    Ass Gcc5>

    Ass Gcc5> :?
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
      T(1,2,3)                  tuple/pair constructor
      P(arg1, arg2, ...)        variadic print
      type_name<type>()         demangle the name of a type
      type_of(v)                deduce the type of a given expression
      R(1,2,5)                  range: initializer_list<int> {1,2,3,4,5}
      S(v)                      stringify a value
      hex(v), oct(v), bin(v)    show manipulators
      class O                   oracle class.

    Using Gcc5 compiler...
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
    Leaving Ass++. 

