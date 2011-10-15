#!/bin/sh
set -e -x

/usr/bin/ghc -Wall ass.hs -o /usr/local/bin/ass

/usr/bin/g++ ass.hpp -std=c++0x -O0 -o /usr/local/include/ass.hpp.gch

/bin/cp ass.hpp /usr/local/include/

