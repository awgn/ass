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


module CppFilter (CppZone, CppZoneFilter, cppFilter)  where


data CppZone = Code | Comment | Literal
                deriving (Show, Read)


type Source = String


type CppZoneFilter = (Bool, Bool, Bool)


cppFilter :: CppZoneFilter -> Source  -> Source
cppFilter = runFilter CodeState 


runFilter :: FilterState -> CppZoneFilter -> Source -> Source
runFilter _ _ [] = []
runFilter state filt (x:n:xs) 
    | zoneFilter region filt   = x   : (runFilter nextState filt (n:xs))
    | otherwise                = ' ' : (runFilter nextState filt (n:xs))
        where (region, nextState) = charFilter (x,n) state
runFilter state filt (x:xs) 
    | zoneFilter region filt   = x   : (runFilter nextState filt xs)
    | otherwise                = ' ' : (runFilter nextState filt xs)
        where (region, nextState) = charFilter (x, ' ') state


data FilterState =  CodeState       | 
                    SlashState      | 
                    AsteriskState   | 
                    CommentCState   | 
                    CommentCppState | 
                    LiteralState
                    deriving (Show, Read)


charFilter :: (Char,Char) -> FilterState -> (CppZone, FilterState)


charFilter (x,n) CodeState 
    | x == '/' && n == '/' = (Comment, SlashState)
    | x == '/' && n == '*' = (Comment, SlashState)
    | x == '"'  = (Literal, LiteralState)
    | otherwise = (Code, CodeState)

charFilter (x,_) SlashState 
    | x == '/'  = (Comment, CommentCppState)
    | x == '*'  = (Comment, CommentCState) 
    | otherwise = error "charFilter"

charFilter (x,_) CommentCppState
    | x == '\n' = (Comment, CodeState)
    | otherwise = (Comment, CommentCppState)

charFilter (x,_) CommentCState
    | x == '*'  = (Comment, AsteriskState)
    | otherwise = (Comment, CommentCState)

charFilter (x,_) AsteriskState
    | x == '/'  = (Comment, CodeState)
    | x == '*'  = (Comment, AsteriskState)
    | otherwise = (Comment, CommentCState)

charFilter (x,_) LiteralState
    | x == '"'  = (Literal, CodeState)
    | otherwise = (Literal, LiteralState)


zoneFilter :: CppZone -> CppZoneFilter -> Bool
zoneFilter Code    (x, _, _) = (x == True)
zoneFilter Comment (_, x, _) = (x == True)
zoneFilter Literal (_, _, x) = (x == True)


