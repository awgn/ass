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

{-# LANGUAGE ViewPatterns #-} 

module Cpp.Filter (Context(..), ContextFilter(..), Cpp.Filter.filter)  where

import qualified Cpp.Source as Cpp
import qualified Data.ByteString.Char8 as C


type Source = Cpp.Source


data Context = Code | Comment | Literal
                deriving (Eq, Show)


data ContextFilter = ContextFilter { getCode    :: Bool,
                                     getComment :: Bool,
                                     getLiteral :: Bool }
                    deriving (Eq, Show)


filter :: ContextFilter -> Source -> Source
filter = runFilter CodeState 


runFilter :: FilterState -> ContextFilter -> Source -> Source
runFilter state filt (C.uncons -> Just (x, C.uncons -> Just (n,xs))) 
    | cxtFilter cxt filt = x `C.cons` (runFilter nextState filt (n `C.cons` xs))
    | otherwise          = charReplace x `C.cons` (runFilter nextState filt (n `C.cons` xs))
        where (cxt, nextState) = charFilter (x,n) state
runFilter state filt (C.uncons -> Just (x,xs)) 
    | cxtFilter cxt filt = x `C.cons` (runFilter nextState filt xs)
    | otherwise          = charReplace x `C.cons` (runFilter nextState filt xs)
        where (cxt, nextState) = charFilter (x, ' ') state
runFilter _ _ (C.uncons -> Nothing) = C.empty
runFilter _ _ _ = C.empty


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


charFilter :: (Char,Char) -> FilterState -> (Context, FilterState)


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


cxtFilter :: Context -> ContextFilter -> Bool
cxtFilter Code    xs = getCode xs
cxtFilter Comment xs = getComment xs
cxtFilter Literal xs = getLiteral xs


