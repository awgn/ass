#!/bin/bash

# Copyright (c) 2011 Bonelli Nicola <bonelli@antifork.org>
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

set -e
set -x 

case `uname` in

Linux)
    ASS="\e[1m[ass]\e[0m"
    CLANG_LIBC=""
    ;;
Darwin)
    ASS="[ass]"
    CLANG_LIBC="-stdlib=libc++"
    ;;
esac

echo -e "${ASS} Installing C++11 assistant."

echo -e "${ASS} Installing rc file..."
/bin/cp assrc $HOME/.assrc

if [ -d $HOME/.vim/bundle/ ]; then
    echo -e "${ASS} Installing vim-ass plug-in (pathogen detected)..."
    /bin/mkdir -p ${HOME}/.vim/bundle/vim-ass/
    cp -r plugin  ${HOME}/.vim/bundle/vim-ass/ 
fi


echo -e "${ASS} Compiling Haskell binaries..."

sudo /usr/bin/ghc -O -Wall Ass.hs -o /usr/local/bin/ass
sudo /usr/bin/ghc -O -Wall Gen.hs -o /usr/local/bin/gen

sudo /bin/ln -f -s /usr/local/bin/ass  /usr/local/bin/ass-clang


echo -e "${ASS} Installing headers..."

sudo /bin/cp includes/ass.hpp    /usr/local/include/
sudo /bin/cp includes/ass-mt.hpp /usr/local/include/


case `uname` in
Linux)

    if [ -x /usr/bin/g++-4.8 ]; then
        echo -e "${ASS} Precompiling headers for g++-4.8..."
        mkdir -p /usr/local/include/4.8
        sudo /usr/bin/g++-4.8 includes/ass.hpp    -std=c++11 -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -o           /usr/local/include/4.8/ass.hpp.gch
        sudo /usr/bin/g++-4.8 includes/ass-mt.hpp -std=c++11 -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -pthread -o  /usr/local/include/4.8/ass-mt.hpp.gch
    fi

    if [ -x /usr/bin/g++-4.7 ]; then
        echo -e "${ASS} Precompiling headers for g++-4.7..."
        mkdir -p /usr/local/include/4.7
        sudo /usr/bin/g++-4.7 includes/ass.hpp    -std=c++11 -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -o           /usr/local/include/4.7/ass.hpp.gch
        sudo /usr/bin/g++-4.7 includes/ass-mt.hpp -std=c++11 -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -pthread -o  /usr/local/include/4.7/ass-mt.hpp.gch
    fi

    if [ -x /usr/bin/g++-4.6 ]; then
        echo -e "${ASS} Precompiling headers for g++-4.6..."
        mkdir -p /usr/local/include/4.6
        sudo /usr/bin/g++-4.6 includes/ass.hpp    -std=c++0x -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -o           /usr/local/include/4.6/ass.hpp.gch
        sudo /usr/bin/g++-4.6 includes/ass-mt.hpp -std=c++0x -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -pthread -o  /usr/local/include/4.6/ass-mt.hpp.gch
    fi
    ;;
esac

if [ -x /usr/bin/clang++ ]; then
    echo -e "${ASS} Precompiling headers for clang++..."

    mkdir -p /usr/local/include/clang
    sudo /usr/bin/clang++ includes/ass.hpp    -std=c++11 ${CLANG_LIBC} -O0 -D_GLIBCXX_DEBUG -Wall -Wextra          -x c++-header -o /usr/local/include/clang/ass.hpp.pch
    sudo /usr/bin/clang++ includes/ass-mt.hpp -std=c++11 ${CLANG_LIBC} -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -pthread -x c++-header -o /usr/local/include/clang/ass-mt.hpp.pch
fi


echo -e "${ASS} done."
