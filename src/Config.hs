--
-- Copyright (c) 2011 Bonelli Nicola <bonelli@antifork.org>
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
-- ass: C++11 code assistant for vim

module Config where

import Ass.Types

-- default compiler list (overridden by ~/.assrc)
--

compilerList :: [Compiler]
compilerList = [
                 Compiler Gcc48   "/usr/bin/g++-4.8" "g++-4.8" [],
                 Compiler Gcc47   "/usr/bin/g++-4.7" "g++-4.7" [],
                 Compiler Gcc46   "/usr/bin/g++-4.6" "g++-4.6" [],
                 Compiler Clang31 "/usr/bin/clang++" "clang++" [],
                 Compiler Clang32 "/usr/bin/clang++" "clang++" [],
                 Compiler Clang33 "/usr/bin/clang++" "clang++" [],
                 Compiler Clang34 "/usr/bin/clang++" "clang++" []
               ]

banner, snippet, assrc, ass_history :: String
tmpDir, includeDir :: FilePath

banner      = "ASSi, version 2.10"
snippet     = "ass-snippet"
tmpDir      =  "/tmp"
includeDir  =  "/usr/local/include"
assrc       =  ".assrc"
ass_history = ".ass_history"


