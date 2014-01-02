ASS
===

ASSi is an interactive C++11 code assistant inspired to GHCi. 

It can be used through vim or interactively as a shell command. It supports gcc and clang compilers, libstdc++, the newer libc++ 
and few boost headers (like the geordi bot). It features precompiled headers and tab completion for commands, files and C++ identifiers. 

C++ statements are evaluated on the fly, possibly using the source code loaded, no matter if it is about a test including the main function or a class declaration.
Namespaces are automatically deducted parsing the code and made available with using-namespace declarations.

In addition, ASSi provides some C++ goodies that allow you to test code quickly. It provides an oracle class and allows to show STL containers, 
tuples, smart pointers, chrono, streamable types etc. 

Type names can be demangled with T function, _ can is an alias for std::make_tuple and std::make_pair, ranges ala Haskell are available through the R function which
generate a proper std::initializer_list.


Help
----

    usage: ass [OPTION] [COMPILER OPT] -- [ARG]
        -i              launch interactive mode
        -v, --version   show version
        -h, --help      print this help


Session
-------
  
    ASSi, version 2.3 :? for help
    Compilers found: /usr/bin/g++-4.8 /usr/bin/g++-4.7 /usr/bin/g++-4.6 /usr/bin/clang++ 
    Using Gcc48 compiler...
    Ass> :?
    Commands available from the prompt:
    
    <statement>                 evaluate/run C++ <statement>
      :include file             add include in the buffer
      :load file                load file in the buffer
      :reload                   reload the file
      :edit                     edit the buffer
      :show                     show the buffer
      :clear                    clear the buffer
      :next                     switch to next compiler
      :args ARG1 ARG2...        set runtime arguments
      :run [ARG1 ARG2...]       run main function
      :preload                  toggle preload std headers
      :quit                     quit
      :?                        print this help

    C++ goodies:
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
    Ass> :show 
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
    Ass> :quit
    Leaving ASSi. 
