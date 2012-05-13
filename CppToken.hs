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


module CppToken(Token(..), tokens)  where
      
import Data.Char

-- Tokenize the source code in a list of Token
--

data Token = NullToken  |
             Identifier  { toString :: String } |
             Keyword     { toString :: String } |
             Number      { toString :: String } |
             HeaderName  { toString :: String } |
             TString     { toString :: String } |
             TChar       { toString :: String } |
             OperOrPunct { toString :: String }
                deriving (Show, Read)


tokens :: String -> [Token]
tokens xs = getTokens xs Null  


-- Requred for Header Name Token
--
data PreprocState = Null | Hash | Include
                    deriving (Show, Eq)


nextState :: String -> PreprocState -> PreprocState
nextState "#" Null       = Hash
nextState "include" Hash = Include
nextState _   _          = Null


getTokens :: String -> PreprocState -> [Token]
getTokens [] _ = []
getTokens xs state = token : getTokens ls (nextState (toString token) next) 
                        where (token, next) = runGetToken xs state
                              ls = dropWhile (\c -> c `elem` whitespace) $ drop (length $ toString token) xs                        


runGetToken :: String -> PreprocState -> (Token, PreprocState)
runGetToken []  s = (NullToken, s)
runGetToken xs  s = case xs' of 
                        (y:_) | y == '0'       -> (getTokenNumber xs', s)
                        (y:_) | y == '"'       -> (getTokenString xs', s)
                        (y:_) | y == '\''      -> (getTokenChar   xs', s)
                        (y:_) | (y == '<') && 
                                (s == Include) -> (getTokenHeaderName  xs', s)
                        (y:_) | isIdentifier y -> (getTokenIdOrKeyword xs', s)
                        _                      -> (getTokenOpOrPunct   xs', s)
                        where
                           xs' = dropWhile (\c -> c `elem` whitespace) xs


getTokenIdOrKeyword, getTokenNumber, getTokenHeaderName, getTokenString, getTokenChar, getTokenOpOrPunct :: String -> Token


getTokenIdOrKeyword xs 
    | name `elem` keywords = Keyword name
    | otherwise            = Identifier name
                where name = takeWhile isIdentifier xs


getTokenNumber      xs = Number     (takeWhile isLiteralNum xs)
getTokenHeaderName  xs = HeaderName (getLiteralDelim '<'  '>'  False xs)
getTokenString      xs = TString    (getLiteralDelim '"'  '"'  False xs)
getTokenChar        xs = TChar      (getLiteralDelim '\'' '\'' False xs)


getTokenOpOrPunct (a:b:c:d:_) 
    | a:b:c:[d] `elem` (oper_or_punct !! 3) = OperOrPunct (a:b:c:[d])
    | a:b:[c]   `elem` (oper_or_punct !! 2) = OperOrPunct (a:b:[c])
    | a:[b]     `elem` (oper_or_punct !! 1) = OperOrPunct (a:[b])
    | a         `elem` (oper_or_punct !! 0 !! 0) = OperOrPunct [a]
    | otherwise  = error "getTokenOpOrPunct"
getTokenOpOrPunct (a:b:c:_) 
    | a:b:[c]   `elem` (oper_or_punct !! 2) = OperOrPunct (a:b:[c])
    | a:[b]     `elem` (oper_or_punct !! 1) = OperOrPunct (a:[b])
    | a         `elem` (oper_or_punct !! 0 !! 0) = OperOrPunct [a]
    | otherwise  = error "getTokenOpOrPunct"
getTokenOpOrPunct (a:b:_) 
    | a:[b]     `elem` (oper_or_punct !! 1) = OperOrPunct (a:[b])
    | a         `elem` (oper_or_punct !! 0 !! 0) = OperOrPunct [a]
    | otherwise  = error "getTokenOpOrPunct"
getTokenOpOrPunct (a:_) 
    | a         `elem` (oper_or_punct !! 0 !! 0) = OperOrPunct [a]
    | otherwise  = error "getTokenOpOrPunct"
getTokenOpOrPunct []  
                 = error "getTokenOpOrPunct" 


getLiteralDelim :: Char -> Char -> Bool -> String -> String
getLiteralDelim _  _  _ []  = []
getLiteralDelim b e False (x : xs)
    | x == b     =  b : getLiteralDelim b e True xs
    | otherwise  = error "getLiteral"
getLiteralDelim  b  e True (x : xs) 
    | x == e     = [e]
    | x == '\\'  = x' : getLiteralDelim b e True xs'
    | otherwise  = x  : getLiteralDelim b e True xs
                    where
                        (x':xs') = xs


isIdentifier, isLiteralNum :: Char -> Bool
isIdentifier c = isAlphaNum c || c == '_'
isLiteralNum c = c `elem` "0123456789abcdefABCDEF.xXeEuUlL"

whitespace :: [Char]
whitespace = " \t\r\n" 

oper_or_punct :: [[[Char]]]
oper_or_punct = [   [ "{}[]#();:?.+-*/%^&|~!=<>," ],
                    ["##", "<:", ":>", "<%", "%>", "%:", "::", ".*", "+=",   
                     "-=", "*=", "/=", "%=", "^=", "&=", "|=", "<<", ">>",   
                     ">=", "<=", "&&", "||", "==", "!=", "++", "--", "->" ],      
                    ["...", "<<=", ">>=", "->*"],   
                    [ "%:%:" ]       
                ]
keywords :: [[Char]]
keywords = ["alignas", "continue", "friend", "alignof", "decltype", "goto",
            "asm", "default", "if", "auto", "delete", "inline", "bool", "do", "int",
            "break", "double", "long", "case", "dynamic_cast", "mutable", "catch", "else", 
            "namespace", "char", "enum", "new", "char16_t", "explicit", "noexcept", "char32_t", 
            "export", "nullptr", "class", "extern", "operator", "const", "false", "private", 
            "constexpr", "float", "protected", "const_cast", "for", "public", "register", "true", 
            "reinterpret_cast", "try", "return", "typedef", "short", "typeid", "signed", "typename", 
            "sizeof", "union", "static", "unsigned", "static_assert", "using", "static_cast", "virtual", 
            "struct", "void", "switch", "volatile", "template", "wchar_t", "this", "while", "thread_local", "throw",
            "and", "and_eq", "bitand", "bitor", "compl", "not", "not_eq", "or", "or_eq", "xor", "xor_eq"]

