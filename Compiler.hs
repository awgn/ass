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
-- ass: C++ code ass'istant

module Compiler where

import System.Process
import System.Exit

type SourceFile  =  String
type OutputFile  =  String
type ExtraOpt    = [String]

-- Executable class

class Executable x where
    filename    :: x -> String
    defaultArgs :: x -> [String]

-- Compiler data type

data Compiler = Gcc | Gxx deriving (Show,Enum) 

instance Executable Compiler where
    filename comp = case comp of 
        (Gcc) -> "/usr/bin/gcc"
        (Gxx) -> "/usr/bin/g++"

    defaultArgs comp = case comp of 
        (Gcc) -> [ "-O0", "-Wall", "-Wextra" ] 
        (Gxx) -> [ "-std=c++0x", "-O0", "-D_GLIBXX_DEBUG", "-Wall", "-Wextra", "-Wno-unused-parameter" ]

-- run compiler

compileWith :: Compiler -> SourceFile -> OutputFile -> ExtraOpt -> IO ExitCode
compileWith c s a o= system (
                        unwords $ [ filename c, s, "-o", a ] ++ defaultArgs c ++ o) 


