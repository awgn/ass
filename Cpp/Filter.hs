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
import qualified Data.ByteString.Lazy.Char8 as C

type Source = Cpp.Source


data State = State {
                cstate  :: ContextState,
                cfilter :: ContextFilter,
                pchar   :: Char
             } deriving (Eq, Show)


data Context = Code | Comment | Literal
                deriving (Eq, Show)


data ContextState = CodeState       | 
                    CommentCState   | 
                    CommentCppState | 
                    LiteralStateS   |
                    LiteralStateC
                        deriving (Eq, Show)


data ContextFilter = ContextFilter { getCode    :: Bool,
                                     getComment :: Bool,
                                     getLiteral :: Bool 
                     } deriving (Eq, Show)


filter :: ContextFilter -> Source -> Source
filter filt src =  snd $ C.mapAccumL runFilter (State CodeState filt ' ') src 


runFilter :: State -> Char -> (State, Char) 
runFilter state c = (state', charFilter (cxtFilter cxt (cfilter state)) cxt c)
                        where (cxt, state') = charParser(pchar state, c) state


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


charParser :: (Char,Char) -> State -> (Context, State)

charParser (p,c) state@(State CodeState _ _) 
    | p == '/'  && c == '/'  = (Code, state { cstate = CommentCppState, pchar = c })
    | p == '/'  && c == '*'  = (Code, state { cstate = CommentCState,   pchar = c })
    | p /= '\\' && c == '"'  = (Code, state { cstate = LiteralStateS,   pchar = c })
    | p /= '\\' && c == '\'' = (Code, state { cstate = LiteralStateC,   pchar = c }) 
    | p == '\\' && c == '\\' = (Code, state { pchar = ' ' })
    | otherwise = (Code, state { pchar = c } )
                                       
charParser (_,c) state@(State CommentCppState _ _)
    | c == '\n' = (Comment, state { cstate = CodeState, pchar = c })
    | otherwise = (Comment, state { pchar = c })

charParser (p,c) state@(State CommentCState _ _)
    | p == '*' && c == '/'  = (Comment, state { cstate = CodeState, pchar = c})
    | otherwise = (Comment, state { pchar = c })

charParser (p,c) state@(State LiteralStateS _ _)
    | p /= '\\' && c == '"'  = (Code,    state { cstate = CodeState, pchar = c})
    | p == '\\' && c == '\\' = (Literal, state { pchar = ' '})
    | otherwise = (Literal, state { pchar = c }) 

charParser (p,c) state@(State LiteralStateC _ _)
    | p /= '\\' && c == '\'' = (Code, state { cstate = CodeState, pchar = c })
    | p == '\\' && c == '\\' = (Literal, state { pchar = ' '})
    | otherwise = (Literal, state { pchar = c})


cxtFilter :: Context -> ContextFilter -> Bool
cxtFilter Code    xs = getCode xs
cxtFilter Comment xs = getComment xs
cxtFilter Literal xs = getLiteral xs


