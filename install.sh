#!/bin/sh
set -e -x

/usr/bin/ghc -O -Wall Ass.hs -o /usr/local/bin/ass
/usr/bin/ghc -O -Wall Gen.hs -o /usr/local/bin/gen

/usr/bin/g++ includes/ass.hpp    -std=c++0x -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -o /usr/local/include/ass.hpp.gch
/usr/bin/g++ includes/ass-mt.hpp -std=c++0x -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -o /usr/local/include/ass-mt.hpp.gch

/bin/cp includes/ass.hpp /usr/local/include/
/bin/cp includes/ass-mt.hpp /usr/local/include/

