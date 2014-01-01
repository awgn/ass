Ass
===

Ass is an interactive C++11 code assistant inspired to GHCi. 

It can be used through vim or as a shell command. It supports gcc and clang compilers, libstdc++ and the newer libc++ 
library using precompiled headers. 

In addition to including the standard headers, it provides some C++ goodies that allow you to test C++ code quickly.

Session
-------

    ASSi, version 2.0 :? for help
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
      :quit                     quit
      :?                        print this help
    
    C++ goodies:
      _(1,2,3)                  tuple/pair constructor
      P(arg1, arg2, ...)        variadic print
      S(instance)               stringify a value
      T<type>()                 demangle the name of a type
      R(1,2,5)                  range: initializer_list<int> {1,2,3,4,5}
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
    Ass> :load test.h 
    loading test.h...
    Ass> :show 
    
    template <typename T>
    using test = vector<T>;
    
    Ass> return test<int>{1,2,3};
    [ 1 2 3 ] [ExitSuccess]
    Ass>
    Ass> :quit
    Leaving ASSi. 
