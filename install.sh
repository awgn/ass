#!/bin/sh
set -x

#/usr/bin/g++ ass.cpp -std=c++0x -Wall -Wextra -o /usr/local/bin/ass 
/usr/bin/ghc ass.hs -o /usr/local/bin/ass
/usr/bin/g++ ass.hpp -std=c++0x -O0 -o /usr/local/include/ass.hpp.gch
/bin/cp ass.hpp /usr/local/include/

