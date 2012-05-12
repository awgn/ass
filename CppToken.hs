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


module CppToken where
      
import Data.Char
import Data.List

-- Requred for Header Name Token
--
data PreprocState = Null | Hash | Include
                    deriving (Show, Eq)

nextState :: String -> PreprocState -> PreprocState
nextState "#" Null       = Hash
nextState "include" Hash = Include
nextState _   _          = Null


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


getTokens :: String -> PreprocState -> [Token]
getTokens [] state = []
getTokens xs state = token : getTokens ls (nextState (toString token) state) 
                        where (token, next) = runGetToken xs state
                              ls = dropWhile (\c -> c `elem` whitespace) $ drop (length $ toString token) xs                        

runGetToken :: String -> PreprocState -> (Token, PreprocState)
runGetToken []  s = (NullToken, s)
runGetToken xs  s = case xs' of 
                        (y:ys) | y == '0'       -> (getTokenNumber xs', s)
                        (y:ys) | y == '"'       -> (getTokenString xs', s)
                        (y:ys) | y == '\''      -> (getTokenChar xs', s)
                        (y:ys) | (y == '<') && 
                                 (s == Include) -> (getTokenHeaderName xs', s)
                        (y:ys) | isIdentifier y -> (getTokenIdOrKeyword xs', s)
                        otherwise               -> (getTokenOpOrPunct xs',s)
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


getTokenOpOrPunct (a:b:c:d:xs) 
    | a:b:c:[d]`elem`oper4 = OperOrPunct (a:b:c:[d])
    | a:b:[c] `elem` oper3 = OperOrPunct (a:b:[c])
    | a:[b]   `elem` oper2 = OperOrPunct (a:[b])
    | a       `elem` oper1 = OperOrPunct [a]
getTokenOpOrPunct (a:b:c:xs) 
    | a:b:[c] `elem` oper3 = OperOrPunct (a:b:[c])
    | a:[b]   `elem` oper2 = OperOrPunct (a:[b])
    | a       `elem` oper1 = OperOrPunct [a]
getTokenOpOrPunct (a:b:xs) 
    | a:[b]   `elem` oper2 = OperOrPunct (a:[b])
    | a       `elem` oper1 = OperOrPunct [a]
getTokenOpOrPunct (a:xs) 
    | a       `elem` oper1 = OperOrPunct [a]
    | otherwise = error "getTokenOpOrPunct"


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

isIdentifier = \c -> isAlphaNum c || c == '_'
isLiteralNum = \c -> c `elem` "0123456789abcdefABCDEF.xXeEuUlL"

whitespace = " \t\r\n"

oper4 = [ "%:%:" ]
oper3 = ["...", "<<=", ">>=", "->*"]
oper2 = ["##", "<:", ":>", "<%", "%>", "%:", "::", ".*", 
         "+=", "-=", "*=", "/=", "%=", "^=", "&=", "|=", 
         "<<", ">>", ">=", "<=", "&&", "||", "==", "!=", 
         "++", "--", "->" ]

oper1 = "{}[]#();:?.+-*/%^&|~!=<>,"

keywords = ["alignas", "continue", "friend", "alignof", "decltype", "goto",
            "asm", "default", "if", "auto", "delete", "inline", "bool", "do", "int",
            "break", "double", "long", "case", "dynamic_cast", "mutable", "catch", "else", 
            "namespace", "char", "enum", "new", "char16_t", "explicit", "noexcept", "char32_t", 
            "export", "nullptr", "class", "extern", "operator", "const", "false", "private", 
            "constexpr", "float", "protected", "const_cast", "for", "public", "register", "true", 
            "reinterpret_cast", "try", "return", "typedef", "short", "typeid", "signed", "typename", 
            "sizeof", "union", "static", "unsigned", "static_assert", "using", "static_cast", "virtual", 
            "struct", "void", "switch", "volatile", "template", "wchar_t", "this", "while", "thread_local", "throw"]

