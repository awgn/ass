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

module Ass.Types where

import qualified Data.ByteString.Char8 as C
import qualified Cpp.Source as Cpp


type Source          = Cpp.Source
data CodeLine        = CodeLine Int Source

instance Show CodeLine where
    show (CodeLine n xs)
        | C.pack "\\" `C.isSuffixOf` xs = C.unpack xs
        | otherwise                     = C.unpack xs ++ "\n#line " ++ show n

type SourceCode      = [CodeLine]
type TranslationUnit = SourceCode
type MainFunction    = SourceCode
type ParserState     = (TranslationUnit, MainFunction)

-- Compiler:

data CompilerType = Gcc46 | Gcc47 | Gcc48 | Gcc49 | Clang31 | Clang32 | Clang33 | Clang34
                    deriving (Eq,Show,Read,Enum)


next :: CompilerType -> CompilerType
next Clang34 = Gcc46
next x = succ x


data CompilerFamily = Gcc | Clang
    deriving (Eq,Show,Read,Enum)


data Compiler = Compiler CompilerType FilePath String [String]
    deriving (Read, Eq)


instance Show Compiler where
    show (Compiler _ _ name opt) = name ++ " (" ++ show opt ++ ")"

