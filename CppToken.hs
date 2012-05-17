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


module CppToken(Token(..), isTIdentifier, isTKeyword, isTDirective, isTNumber, 
                           isTHeaderName, isTString, isTChar, isTOperOrPunct, tokens)  where
      
import Data.Char 
import Data.Set as S

-- Tokenize the source code in a list 
-- Precondition: the c++ source code must not be not ill-formed
--

data Token = TIdentifier  { toString :: String } |
             TDirective   { toString :: String } |
             TKeyword     { toString :: String } |
             TNumber      { toString :: String } |
             THeaderName  { toString :: String } |
             TString      { toString :: String } |
             TChar        { toString :: String } |
             TOperOrPunct { toString :: String }
                deriving (Show, Read)

isTIdentifier :: Token -> Bool
isTIdentifier (TIdentifier _)  = True
isTIdentifier _ = False

isTKeyword :: Token -> Bool
isTKeyword (TKeyword _)  = True
isTKeyword _ = False

isTDirective :: Token -> Bool
isTDirective (TDirective _)  = True
isTDirective _ = False

isTNumber :: Token -> Bool
isTNumber (TNumber _) = True
isTNumber _ = False

isTHeaderName :: Token -> Bool
isTHeaderName (THeaderName _)  = True
isTHeaderName _ = False

isTString :: Token -> Bool
isTString (TString _ ) = True
isTString _ = False

isTChar :: Token -> Bool
isTChar (TChar _) = True
isTChar _ = False

isTOperOrPunct :: Token -> Bool
isTOperOrPunct (TOperOrPunct _)  = True
isTOperOrPunct _ = False


tokens :: String -> [Token]
tokens xs = getTokens (dropWhile (\c -> c `S.member` whitespace) xs) Null  

-- 

data PreprocState = Null | Hash | Include | Define | Undef | If | Ifdef | Ifndef | Elif | Else | Endif |
                    Line | Error | Pragma
                    deriving (Show, Eq)


nextState :: String -> PreprocState -> PreprocState
nextState "#" Null       = Hash
nextState "include" Hash = Include
nextState "define"  Hash = Define
nextState "undef"   Hash = Undef
nextState "if"      Hash = If 
nextState "ifdef"   Hash = Ifdef 
nextState "ifndef"  Hash = Ifndef 
nextState "elif"    Hash = Elif 
nextState "else"    Hash = Else 
nextState "endif"   Hash = Endif 
nextState "line"    Hash = Line  
nextState "error"   Hash = Error  
nextState "pragma"  Hash = Pragma
nextState _   _          = Null


getTokens :: String -> PreprocState -> [Token]
getTokens [] _ = []
getTokens xs state = token : getTokens ls (nextState (toString token) next) 
                        where (token, next) = runGetToken xs state
                              ls = dropWhile (\c -> c `S.member` whitespace) $ drop (length $ toString token) xs                        


runGetToken :: String -> PreprocState -> (Token, PreprocState)
runGetToken []  _ = error "runGetToken"
runGetToken xs  s = case xs' of 
                        _ | s == Hash                  -> (getTokenDirective xs', s)
                        _ | s == Include               -> (getTokenHeaderName  xs', s)
                        (y:_) | isDigit(y)             -> (getTokenNumber xs', s)
                        (y:_) | isAlpha(y) || y == '_' -> (getTokenIdOrKeyword xs', s)
                        (y:_) | y == '"'               -> (getTokenString xs', s)
                        (y:_) | y == '\''              -> (getTokenChar   xs', s)
                        _                              -> (getTokenOpOrPunct   xs', s)
                        where
                           xs' = dropWhile (\c -> c `S.member` whitespace) xs


getTokenIdOrKeyword, getTokenNumber, getTokenHeaderName, 
    getTokenString, getTokenChar, getTokenOpOrPunct, getTokenDirective :: String -> Token


getTokenIdOrKeyword xs 
    | name `S.member` keywords = TKeyword name
    | otherwise            = TIdentifier name
                where name = takeWhile (\c -> isAlphaNum c || c == '_') xs


getTokenDirective xs  = TDirective name
                        where name = takeWhile (\c -> isAlphaNum c)  xs

getTokenNumber      xs = TNumber  (takeWhile (\c -> c `S.member` S.fromList "0123456789abcdefABCDEF.xXeEuUlL" )  xs)
getTokenString      xs = TString  (getLiteral '"'  '"'  False xs)
getTokenChar        xs = TChar    (getLiteral '\'' '\'' False xs)


getTokenHeaderName  xs@(y:_)
    | y == '<'  = THeaderName (getLiteral '<'  '>'  False xs)
    | y == '"'  = THeaderName (getLiteral '"'  '"'  False xs)
    | otherwise = error "getTokenHeaderName"
getTokenHeaderName [] =  error "getTokenHeaderName"


getLiteral :: Char -> Char -> Bool -> String -> String
getLiteral _  _  _ []  = []
getLiteral b e False (x : xs)
    | x == b     =  b : getLiteral b e True xs
    | otherwise  = error "getLiteral"
getLiteral b e True (x : xs) 
    | x == e     = [e]
    | x == '\\'  = '\\' : x' : getLiteral b e True xs' 
    | otherwise  = x  : getLiteral b e True xs
                    where
                        (x':xs') = xs

getTokenOpOrPunct (a:b:c:d:_) 
    | (a:b:c:[d]) `S.member` (oper_or_punct !! 3) = TOperOrPunct (a:b:c:[d])
    | (a:b:[c])   `S.member` (oper_or_punct !! 2) = TOperOrPunct (a:b:[c])
    | (a:[b])     `S.member` (oper_or_punct !! 1) = TOperOrPunct (a:[b])
    | ([a])       `S.member` (oper_or_punct !! 0) = TOperOrPunct [a]
    | otherwise  = error "getTokenOpOrPunct"
getTokenOpOrPunct (a:b:c:_) 
    | (a:b:[c])   `S.member` (oper_or_punct !! 2) = TOperOrPunct (a:b:[c])
    | (a:[b])     `S.member` (oper_or_punct !! 1) = TOperOrPunct (a:[b])
    | ([a])       `S.member` (oper_or_punct !! 0) = TOperOrPunct [a]
    | otherwise  = error "getTokenOpOrPunct"
getTokenOpOrPunct (a:b:_) 
    | (a:[b])     `S.member` (oper_or_punct !! 1) = TOperOrPunct (a:[b])
    | ([a])       `S.member` (oper_or_punct !! 0) = TOperOrPunct [a]
    | otherwise  = error "getTokenOpOrPunct"
getTokenOpOrPunct (a:_) 
    | ([a])       `S.member` (oper_or_punct !! 0) = TOperOrPunct [a]
    | otherwise  = error "getTokenOpOrPunct"
getTokenOpOrPunct []  
                 = error "getTokenOpOrPunct" 


whitespace = S.fromList " \t\r\n" 


oper_or_punct = [   S.fromList  [ "{","}","[","]","#","(",")",";",":","?",".","+","-","*",
                                  "/","%","^","&","|","~","!","=","<",">","," ],
                    S.fromList  [ "##", "<:", ":>", "<%", "%>", "%:", "::", ".*", "+=", "-=", 
                                  "*=", "/=", "%=", "^=", "&=", "|=", "<<", ">>", ">=", "<=", 
                                  "&&", "||", "==", "!=", "++", "--", "->", "//", "/*", "*/"],      
                    S.fromList  [ "...", "<<=", ">>=", "->*"],   
                    S.fromList  [ "%:%:" ]       
                ]

keywords = S.fromList ["alignas", "continue", "friend", "alignof", "decltype", "goto", "asm", 
                       "default", "if", "auto", "delete", "inline", "bool", "do", "int", "break", 
                       "double", "long", "case", "dynamic_cast", "mutable", "catch", "else", 
                       "namespace", "char", "enum", "new", "char16_t", "explicit", "noexcept", 
                       "char32_t", "export", "nullptr", "class", "extern", "operator", "const", 
                       "false", "private", "constexpr", "float", "protected", "const_cast", "for", 
                       "public", "register", "true", "reinterpret_cast", "try", "return", "typedef", 
                       "short", "typeid", "signed", "typename", "sizeof", "union", "static", "unsigned", 
                       "static_assert", "using", "static_cast", "virtual", "struct", "void", "switch", 
                       "volatile", "template", "wchar_t", "this", "while", "thread_local", "throw", 
                       "and", "and_eq", "bitand", "bitor", "compl", "not", "not_eq", "or", "or_eq", 
                       "xor", "xor_eq"]
                        

