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


INSTALL_ASS=Y
INSTALL_VPI=N
INSTALL_PCH=N


usage()
{
    echo "Usage: $0 [-l] [-v] [-a] [-h]" 1>&2; exit 1;
}


while getopts pvah opt
 do
  case "$opt" in
    v)		INSTALL_VPI=Y;;
    p)		INSTALL_PCH=Y;;
    a)      INSTALL_VPI=Y; INSTALL_PCH=Y;;
    h)      usage $0;;
    *)      exit 1;;
  esac
 done


 echo -e "${ASS} Installing C++11 Assistant."

if [ $INSTALL_VPI = "Y" ];
then

    if [ -d $HOME/.vim/bundle/ ]; then
        echo -e "${ASS} Installing vim-ass plug-in (pathogen detected)..."
        /bin/mkdir -p ${HOME}/.vim/bundle/vim-ass/
        cp -r plugin  ${HOME}/.vim/bundle/vim-ass/ 
    fi
fi

echo -e "${ASS} Compiling Haskell binaries..."

cd src;

sudo /usr/bin/ghc --make -O -Wall Ass.hs -o /usr/local/bin/ass
sudo /usr/bin/ghc --make -O -Wall Gen.hs -o /usr/local/bin/gen

cd -

sudo /bin/ln -f -s /usr/local/bin/ass  /usr/local/bin/ass-clang

echo -e "${ASS} Installing rc file..."
/bin/cp assrc $HOME/.assrc

echo -e "${ASS} Installing headers..."

sudo /bin/cp includes/ass.hpp          /usr/local/include/
sudo /bin/cp includes/ass-boost.hpp    /usr/local/include/

if [ $INSTALL_PCH = "Y" ];
then

case `uname` in
Linux)

    if [ -x /usr/bin/g++-4.8 ]; then
        echo -e "${ASS} Precompiling headers for g++-4.8..."
        mkdir -p /usr/local/include/4.8
        
        sudo /usr/bin/g++-4.8 includes/ass.hpp -std=c++11 -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -pthread -o        /usr/local/include/4.8/ass.hpp.gch
        if [ -d "/usr/include/boost" ]; then 
            sudo /usr/bin/g++-4.8 includes/ass-boost.hpp -std=c++11 -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -pthread -o  /usr/local/include/4.8/ass-boost.hpp.gch
        fi
    fi

    if [ -x /usr/bin/g++-4.7 ]; then
        echo -e "${ASS} Precompiling headers for g++-4.7..."
        mkdir -p /usr/local/include/4.7
        
        sudo /usr/bin/g++-4.7 includes/ass.hpp -std=c++11 -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -pthread -o    /usr/local/include/4.7/ass.hpp.gch
        if [ -d "/usr/include/boost" ]; then 
            sudo /usr/bin/g++-4.7 includes/ass-boost.hpp -std=c++11 -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -pthread -o    /usr/local/include/4.7/ass-boost.hpp.gch
        fi
    fi

    if [ -x /usr/bin/g++-4.6 ]; then
        echo -e "${ASS} Precompiling headers for g++-4.6..."
        mkdir -p /usr/local/include/4.6

        sudo /usr/bin/g++-4.6 includes/ass.hpp -std=c++0x -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -pthread -o  /usr/local/include/4.6/ass.hpp.gch
        if [ -d "/usr/include/boost" ]; then 
            sudo /usr/bin/g++-4.6 includes/ass-boost.hpp -std=c++0x -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -pthread -o  /usr/local/include/4.6/ass-boost.hpp.gch
        fi
    fi

    ;;
esac

if [ -x /usr/bin/clang++ ]; then

    if [ ! -z "$CLANG_LIBC" ] || [ -d "/usr/include/c++/v1" ]; then
        echo -e "${ASS} Precompiling headers for clang++... (libc++)"
        mkdir -p /usr/local/include/clang-libc++
        mkdir -p /usr/local/include/clang-libc++1y

        sudo /usr/bin/clang++ includes/ass.hpp -std=c++11 -stdlib=libc++ -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -pthread -x c++-header -o /usr/local/include/clang-libc++/ass.hpp.pch
        if [ -d "/usr/include/boost" ]; then 
        sudo /usr/bin/clang++ includes/ass-boost.hpp -std=c++11 -stdlib=libc++ -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -pthread -x c++-header -o /usr/local/include/clang-libc++/ass-boost.hpp.pch
        fi
        
        if /usr/bin/clang++ --version | grep -q 3.4; then
            echo -e "${ASS} Precompiling headers for clang++1y... (libc++)"
            sudo /usr/bin/clang++ includes/ass.hpp -std=c++1y -stdlib=libc++ -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -pthread -x c++-header -o /usr/local/include/clang-libc++1y/ass.hpp.pch
            if [ -d "/usr/include/boost" ]; then 
            sudo /usr/bin/clang++ includes/ass-boost.hpp -std=c++1y -stdlib=libc++ -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -pthread -x c++-header -o /usr/local/include/clang-libc++1y/ass-boost.hpp.pch
            fi
        fi
    fi 

    if [ -z "$CLANG_LIBC" ]; then
        echo -e "${ASS} Precompiling headers for clang++... (glibcxx)"
        mkdir -p /usr/local/include/clang

        sudo /usr/bin/clang++ includes/ass.hpp -std=c++11 -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -pthread -x c++-header -o /usr/local/include/clang/ass.hpp.pch
        if [ -d "/usr/include/boost" ]; then 
        sudo /usr/bin/clang++ includes/ass-boost.hpp -std=c++11 -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -pthread -x c++-header -o /usr/local/include/clang/ass-boost.hpp.pch
        fi
    fi
fi

fi

echo -e "${ASS} done."
