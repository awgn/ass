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
-- ass: C++11 code ass'istant 


module CppFilter (CppRegion, CppRegionFilter, runSourceFilter)  where


data CppRegion = Code | Comment | Literal
                deriving (Show, Read)


type CppRegionFilter = (Bool, Bool, Bool)


runSourceFilter :: String -> CppRegionFilter -> String
runSourceFilter xs filt = sourceCodeFilter xs filt CodeState 


sourceCodeFilter :: String -> CppRegionFilter -> FilterState  -> String
sourceCodeFilter [] _ _ = []
sourceCodeFilter (x:xs) filt state
    | cppRegionFilter region filt = x   : (sourceCodeFilter xs filt nextState)
    | otherwise                   = ' ' : (sourceCodeFilter xs filt nextState)
        where (region, nextState) = cppFilter x state
            

data FilterState = CodeState | SlashState | AsteriskState | CommentCState | CommentCppState | LiteralState
                    deriving (Show, Read)


cppFilter :: Char -> FilterState -> (CppRegion, FilterState)


cppFilter x CodeState 
    | x == '/'  = (Code, SlashState)
    | x == '"'  = (Code, LiteralState)
    | otherwise = (Code, CodeState)

cppFilter x SlashState 
    | x == '/'  = (Code, CommentCppState)
    | x == '*'  = (Code, CommentCState) 
    | otherwise = cppFilter x CodeState

cppFilter x CommentCppState
    | x == '\n' = (Comment, CodeState)
    | otherwise = (Comment, CommentCppState)

cppFilter x CommentCState
    | x == '*'  = (Comment, AsteriskState)
    | otherwise = (Comment, CommentCState)

cppFilter x AsteriskState
    | x == '/'  = (Code,    CodeState)
    | x == '*'  = (Comment, AsteriskState)
    | otherwise = (Comment, CommentCState)

cppFilter x LiteralState
    | x == '"'  = (Code,    CodeState)
    | otherwise = (Literal, LiteralState)


cppRegionFilter :: CppRegion -> CppRegionFilter -> Bool
cppRegionFilter Code    (x, _, _) = (x == True)
cppRegionFilter Comment (_, x, _) = (x == True)
cppRegionFilter Literal (_, _, x) = (x == True)


