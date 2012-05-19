--
-- Copyright (c) 2012 Bonelli Nicola <bonelli@antifork.org>
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


module CppFilter (CppZone, CppZoneFilter, runSourceFilter)  where


data CppZone = Code | Comment | Literal
                deriving (Show, Read)


type CppZoneFilter = (Bool, Bool, Bool)


runSourceFilter :: String -> CppZoneFilter -> String
runSourceFilter xs filt = sourceCodeFilter xs filt CodeState 


sourceCodeFilter :: String -> CppZoneFilter -> FilterState  -> String
sourceCodeFilter [] _ _ = []
sourceCodeFilter (x:n:xs) filt state
    | cppZoneFilter region filt   = x   : (sourceCodeFilter (n:xs) filt nextState)
    | otherwise                   = ' ' : (sourceCodeFilter (n:xs) filt nextState)
        where (region, nextState) = cppFilter (x,n) state
sourceCodeFilter (x:xs) filt state
    | cppZoneFilter region filt   = x   : (sourceCodeFilter xs filt nextState)
    | otherwise                   = ' ' : (sourceCodeFilter xs filt nextState)
        where (region, nextState) = cppFilter (x, ' ') state


data FilterState = CodeState | SlashState | AsteriskState | CommentCState | CommentCppState | LiteralState
                    deriving (Show, Read)


cppFilter :: (Char,Char) -> FilterState -> (CppZone, FilterState)


cppFilter (x,n) CodeState 
    | x == '/' && n == '/' = (Comment, SlashState)
    | x == '/' && n == '*' = (Comment, SlashState)
    | x == '"'  = (Literal, LiteralState)
    | otherwise = (Code, CodeState)

cppFilter (x,_) SlashState 
    | x == '/'  = (Comment, CommentCppState)
    | x == '*'  = (Comment, CommentCState) 
    | otherwise = error "cppFilter"

cppFilter (x,_) CommentCppState
    | x == '\n' = (Comment, CodeState)
    | otherwise = (Comment, CommentCppState)

cppFilter (x,_) CommentCState
    | x == '*'  = (Comment, AsteriskState)
    | otherwise = (Comment, CommentCState)

cppFilter (x,_) AsteriskState
    | x == '/'  = (Comment, CodeState)
    | x == '*'  = (Comment, AsteriskState)
    | otherwise = (Comment, CommentCState)

cppFilter (x,_) LiteralState
    | x == '"'  = (Literal, CodeState)
    | otherwise = (Literal, LiteralState)


cppZoneFilter :: CppZone -> CppZoneFilter -> Bool
cppZoneFilter Code    (x, _, _) = (x == True)
cppZoneFilter Comment (_, x, _) = (x == True)
cppZoneFilter Literal (_, _, x) = (x == True)


