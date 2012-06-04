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
                           isTHeaderName, isTString, isTChar, isTOperOrPunct, 
                           tokens)  where
      
import Data.Char 
import Data.Set as S

-- Tokenize the source code in a list 
-- Precondition: the c++ source code must not be not ill-formed
--

tokens :: String -> [Token]
tokens xs = runGetToken (ys, n, Null)  
                where
                    (ys,n) = dropWhite xs

data Token = TIdentifier  { toString :: String, offset :: Int } |
             TDirective   { toString :: String, offset :: Int } |
             TKeyword     { toString :: String, offset :: Int } |
             TNumber      { toString :: String, offset :: Int } |
             THeaderName  { toString :: String, offset :: Int } |
             TString      { toString :: String, offset :: Int } |
             TChar        { toString :: String, offset :: Int } |
             TOperOrPunct { toString :: String, offset :: Int }
                deriving (Show, Eq)  


isTIdentifier :: Token -> Bool
isTIdentifier (TIdentifier _ _)  = True
isTIdentifier _ = False


isTKeyword :: Token -> Bool
isTKeyword (TKeyword _ _)  = True
isTKeyword _ = False


isTDirective :: Token -> Bool
isTDirective (TDirective _ _)  = True
isTDirective _ = False


isTNumber :: Token -> Bool
isTNumber (TNumber _ _) = True
isTNumber _ = False


isTHeaderName :: Token -> Bool
isTHeaderName (THeaderName _ _)  = True
isTHeaderName _ = False


isTString :: Token -> Bool
isTString (TString _ _) = True
isTString _ = False


isTChar :: Token -> Bool
isTChar (TChar _ _) = True
isTChar _ = False


isTOperOrPunct :: Token -> Bool
isTOperOrPunct (TOperOrPunct _ _)  = True
isTOperOrPunct _ = False


-- Drop leading whitespace and count them
--

dropWhite :: String -> (String,Int)

dropWhite xs = dropWhite' (xs, 0)
               where dropWhite' (y:ys, n) 
                        | y `elem` " \t\n\\" =  dropWhite' (ys, n+1)
                        | otherwise = (y:ys, n)
                     dropWhite' ("",n) = ("", n)
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

---

type TokenizerState = (Source, Offset, PreprocState)
type Source = String
type Offset = Int


runGetToken :: TokenizerState -> [Token]

runGetToken ([], _, _)     = []
runGetToken ts = token : runGetToken ns
                    where (token, ns) = getToken ts


getToken :: TokenizerState -> (Token, TokenizerState)

getToken ([], _, _ ) = error "getToken"
getToken (xs, n, ps) =  let token = case xs of
                                _ | ps == Hash                 -> (getTokenDirective   xs) { offset = n }
                                _ | ps == Include              -> (getTokenHeaderName  xs) { offset = n }
                                (y:_) | isDigit(y)             -> (getTokenNumber      xs) { offset = n }
                                (y:_) | isAlpha(y) || y == '_' -> (getTokenIdOrKeyword xs) { offset = n }
                                (y:_) | y == '"'               -> (getTokenString      xs) { offset = n }
                                (y:_) | y == '\''              -> (getTokenChar        xs) { offset = n }
                                _                              -> (getTokenOpOrPunct   xs) { offset = n }
                            l = length $ toString token
                            (xs', w) = dropWhite $ drop l xs  
                        in
                            (token, (xs', n+l+w, nextState (toString token) ps))
--

getTokenIdOrKeyword, getTokenNumber, getTokenHeaderName, 
    getTokenString, getTokenChar, getTokenOpOrPunct, getTokenDirective :: Source -> Token


getTokenIdOrKeyword xs 
    | name `S.member` keywords = TKeyword name 0
    | otherwise            = TIdentifier name 0
                where name = takeWhile (\c -> isAlphaNum c || c == '_') xs


getTokenDirective xs  = TDirective name 0
                        where name = takeWhile (\c -> isAlphaNum c)  xs

getTokenNumber      xs = TNumber  (takeWhile (\c -> c `S.member` S.fromList "0123456789abcdefABCDEF.xXeEuUlL" )  xs) 0
getTokenString      xs = TString  (getLiteral '"'  '"'  False xs) 0
getTokenChar        xs = TChar    (getLiteral '\'' '\'' False xs) 0


getTokenHeaderName  xs@(y:_)
    | y == '<'  = THeaderName (getLiteral '<'  '>'  False xs) 0
    | y == '"'  = THeaderName (getLiteral '"'  '"'  False xs) 0
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
    | (a:b:c:[d]) `S.member` (operOrPunct !! 3) = TOperOrPunct (a:b:c:[d]) 0
    | (a:b:[c])   `S.member` (operOrPunct !! 2) = TOperOrPunct (a:b:[c]) 0
    | (a:[b])     `S.member` (operOrPunct !! 1) = TOperOrPunct (a:[b]) 0
    | ([a])       `S.member` (operOrPunct !! 0) = TOperOrPunct [a] 0
    | otherwise  = error $ "getTokenOpOrPunct -> " ++ (show $ a:b:c:[d]) 
getTokenOpOrPunct (a:b:c:_) 
    | (a:b:[c])   `S.member` (operOrPunct !! 2) = TOperOrPunct (a:b:[c]) 0
    | (a:[b])     `S.member` (operOrPunct !! 1) = TOperOrPunct (a:[b]) 0
    | ([a])       `S.member` (operOrPunct !! 0) = TOperOrPunct [a] 0
    | otherwise  = error $ "getTokenOpOrPunct -> " ++ (show $ a:b:[c])
getTokenOpOrPunct (a:b:_) 
    | (a:[b])     `S.member` (operOrPunct !! 1) = TOperOrPunct (a:[b]) 0
    | ([a])       `S.member` (operOrPunct !! 0) = TOperOrPunct [a] 0
    | otherwise  = error $ "getTokenOpOrPunct -> " ++ (show $ a:[b])
getTokenOpOrPunct (a:_) 
    | ([a])       `S.member` (operOrPunct !! 0) = TOperOrPunct [a] 0
    | otherwise  = error $ "getTokenOpOrPunct -> " ++ (show $ [a])
getTokenOpOrPunct []  
                 = error "getTokenOpOrPunct" 


operOrPunct :: [ S.Set String ]
operOrPunct = [   S.fromList  [ "{","}","[","]","#","(",")",";",":","?",".","+","-","*",
                                "/","%","^","&","|","~","!","=","<",">","," ],
                  S.fromList  [ "##", "<:", ":>", "<%", "%>", "%:", "::", ".*", "+=", "-=", 
                                "*=", "/=", "%=", "^=", "&=", "|=", "<<", ">>", ">=", "<=", 
                                "&&", "||", "==", "!=", "++", "--", "->", "//", "/*", "*/"],      
                  S.fromList  [ "...", "<<=", ">>=", "->*"],   
                  S.fromList  [ "%:%:" ]       
               ]

keywords :: S.Set String
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
                        
