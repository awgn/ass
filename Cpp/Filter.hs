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


module Cpp.Filter (Zone(..), ZoneFilter, Cpp.Filter.filter)  where


data Zone = Code | Comment | Literal
                deriving (Eq, Show)


type Source = String


type ZoneFilter = (Bool, Bool, Bool)


filter :: ZoneFilter -> Source  -> Source
filter = runFilter CodeState 


runFilter :: FilterState -> ZoneFilter -> Source -> Source
runFilter _ _ [] = []
runFilter state filt (x:n:xs) 
    | zoneFilter region filt   = x   : (runFilter nextState filt (n:xs))
    | otherwise                = charReplace x : (runFilter nextState filt (n:xs))
        where (region, nextState) = charFilter (x,n) state
runFilter state filt (x:xs) 
    | zoneFilter region filt   = x   : (runFilter nextState filt xs)
    | otherwise                = charReplace x : (runFilter nextState filt xs)
        where (region, nextState) = charFilter (x, ' ') state


charReplace :: Char -> Char
charReplace '\n' = '\n'
charReplace  _   = ' '

data FilterState =  CodeState       | 
                    SlashState      | 
                    AsteriskState   | 
                    CommentCState   | 
                    CommentCppState | 
                    LiteralState
                    deriving (Eq, Show)


charFilter :: (Char,Char) -> FilterState -> (Zone, FilterState)


charFilter (x,n) CodeState 
    | x == '/' && n == '/' = (Comment, SlashState)
    | x == '/' && n == '*' = (Comment, SlashState)
    | x == '"'  = (Code, LiteralState)
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
    | x == '"'  = (Code, CodeState)
    | otherwise = (Literal, LiteralState)


zoneFilter :: Zone -> ZoneFilter -> Bool
zoneFilter Code    (x, _, _) = x
zoneFilter Comment (_, x, _) = x
zoneFilter Literal (_, _, x) = x

