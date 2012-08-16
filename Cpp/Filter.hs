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
type State  = (Char, ContextState, ContextFilter)


data Context = Code | Comment | Literal
                deriving (Eq, Show)


data ContextFilter = ContextFilter { getCode    :: Bool,
                                     getComment :: Bool,
                                     getLiteral :: Bool }
                    deriving (Eq, Show)


filter :: ContextFilter -> Source -> Source
filter filt src =  snd $ C.mapAccumL runFilter (' ', CodeState, filt) src 


runFilter :: State -> Char -> (State, Char) 
runFilter (p, state, filt) c = ((p', state', filt), charFilter (cxtFilter cxt filt) cxt c)
                                where (cxt, state', p') = charParser(p, c) state


charFilter :: Bool -> Context -> Char -> Char
charFilter  _ _ '\n' = '\n'
charFilter  cond _ c
    | cond = c
    | otherwise = ' '


-- debugFilter :: Bool -> Context -> Char -> Char
-- debugFilter  _ _ '\n' = '\n'
-- debugFilter  _ cxt _
--     | Code    <- cxt = '#'
--     | Comment <- cxt = '*'
--     | Literal <- cxt = '_'


data ContextState = CodeState       | 
                    CommentCState   | 
                    CommentCppState | 
                    LiteralStateS   |
                    LiteralStateC
                    deriving (Eq, Show)


charParser :: (Char,Char) -> ContextState -> (Context, ContextState, Char)

charParser (p,c) CodeState 
    | p == '/'  && c == '/'  = (Code, CommentCppState, c)
    | p == '/'  && c == '*'  = (Code, CommentCState,   c)
    | p /= '\\' && c == '"'  = (Code, LiteralStateS,   c)
    | p /= '\\' && c == '\'' = (Code, LiteralStateC,   c) 
    | p == '\\' && c == '\\' = (Code, CodeState,     ' ')
    | otherwise = (Code, CodeState, c)
                                       
charParser (_,c) CommentCppState
    | c == '\n' = (Comment, CodeState, c)
    | otherwise = (Comment, CommentCppState, c)

charParser (p,c) CommentCState
    | p == '*' && c == '/'  = (Comment, CodeState, c)
    | otherwise = (Comment, CommentCState, c)

charParser (p,c) LiteralStateS
    | p /= '\\' && c == '"'  = (Code, CodeState, c)
    | p == '\\' && c == '\\' = (Literal, LiteralStateS, ' ')
    | otherwise = (Literal, LiteralStateS, c) 

charParser (p,c) LiteralStateC
    | p /= '\\' && c == '\'' = (Code, CodeState, c)
    | p == '\\' && c == '\\' = (Literal, LiteralStateC, ' ')
    | otherwise = (Literal, LiteralStateC, c)


cxtFilter :: Context -> ContextFilter -> Bool
cxtFilter Code    xs = getCode xs
cxtFilter Comment xs = getComment xs
cxtFilter Literal xs = getLiteral xs


