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


module Cpp.Token(Token(..), isTIdentifier, isTKeyword, isTDirective, isTNumber, 
                           isTHeaderName, isTString, isTChar, isTOperOrPunct, 
                           tokens)  where
      
import Data.Char 
import Data.Maybe
import Data.Set as S
import Control.Monad

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
dropWhite xs = (xs', count)
                where xs' = dropWhile (`elem` " \t\n\\") xs
                      count = (length xs) - length (xs')

-- 

data State = Null | Hash | Include | Define | Undef | If | Ifdef | Ifndef | Elif | Else | Endif |
                    Line | Error | Pragma
                    deriving (Show, Eq)


nextState :: String -> State -> State
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

type TokenizerState = (Source, Offset, State)
type Source = String
type Offset = Int


runGetToken :: TokenizerState -> [Token]

runGetToken ([], _, _)     = []
runGetToken ts = token : runGetToken ns
                    where (token, ns) = getToken ts


getToken :: TokenizerState -> (Token, TokenizerState)

getToken ([], _, _) = error "getToken"
getToken (xs, off, state) = let token = fromJust $ getTokenDirective xs state   `mplus`
                                                   getTokenHeaderName xs state  `mplus`
                                                   getTokenNumber xs state      `mplus`
                                                   getTokenIdOrKeyword xs state `mplus`
                                                   getTokenString xs state      `mplus`
                                                   getTokenChar xs state        `mplus`
                                                   getTokenOpOrPunct xs state
                                len = length $ toString token
                                (xs', w) = dropWhite $ drop len xs
                            in
                                (token { offset = off }, (xs', off + len + w, nextState(toString token) state))


getTokenIdOrKeyword, getTokenNumber, getTokenHeaderName, 
     getTokenString, getTokenChar, getTokenOpOrPunct, getTokenDirective :: Source -> State -> Maybe Token


getTokenDirective xs  state 
    | state == Hash = Just (TDirective name 0)
    | otherwise = Nothing
                      where name = takeWhile (\c -> isAlphaNum c)  xs

getTokenHeaderName  xs@(x:_) state 
    | state /= Include  = Nothing
    | x == '<'          = Just $ THeaderName (getLiteral '<'  '>'  False xs) 0
    | x == '"'          = Just $ THeaderName (getLiteral '"'  '"'  False xs) 0
    | otherwise         = error $ "getTokenHeaderName: error near " ++ xs
getTokenHeaderName [] _ = error "getTokenHeaderName"

getTokenNumber [] _ = Nothing
getTokenNumber xs@(x:_) _
    | isDigit x = Just $ TNumber  (takeWhile (\c -> c `S.member` S.fromList "0123456789abcdefABCDEF.xXeEuUlL" )  xs) 0
    | otherwise = Nothing

getTokenString [] _ = Nothing
getTokenString xs@(x:_) _
    | x == '"' = Just $ TString (getLiteral '"'  '"'  False xs) 0
    | otherwise = Nothing

getTokenChar [] _ = Nothing
getTokenChar xs@(x:_) _
    | x == '\'' = Just $ TChar  (getLiteral '\'' '\'' False xs) 0
    | otherwise = Nothing

getTokenIdOrKeyword [] _      = Nothing
getTokenIdOrKeyword xs@(x:_) _
    | not $ isIdentifierChar x = Nothing 
    | name `S.member` keywords = Just $ TKeyword name 0
    | otherwise                = Just $ TIdentifier name 0
                                    where isIdentifierChar = (\c -> isAlphaNum c || c == '_') 
                                          name = takeWhile isIdentifierChar xs

getTokenOpOrPunct [] _ = Nothing
getTokenOpOrPunct (a:b:c:d:_) _
    | (a:b:c:[d]) `S.member` (operOrPunct !! 3) = Just $ TOperOrPunct (a:b:c:[d]) 0
    | (a:b:[c])   `S.member` (operOrPunct !! 2) = Just $ TOperOrPunct (a:b:[c]) 0
    | (a:[b])     `S.member` (operOrPunct !! 1) = Just $ TOperOrPunct (a:[b]) 0
    | ([a])       `S.member` (operOrPunct !! 0) = Just $ TOperOrPunct [a] 0
    | otherwise  = error $ "getTokenOpOrPunct: error -> " ++ (show $ a:b:c:[d]) 
getTokenOpOrPunct (a:b:c:_) _
    | (a:b:[c])   `S.member` (operOrPunct !! 2) = Just $ TOperOrPunct (a:b:[c]) 0
    | (a:[b])     `S.member` (operOrPunct !! 1) = Just $ TOperOrPunct (a:[b]) 0
    | ([a])       `S.member` (operOrPunct !! 0) = Just $ TOperOrPunct [a] 0
    | otherwise  = error $ "getTokenOpOrPunct: error -> " ++ (show $ a:b:[c])
getTokenOpOrPunct (a:b:_) _ 
    | (a:[b])     `S.member` (operOrPunct !! 1) = Just $ TOperOrPunct (a:[b]) 0
    | ([a])       `S.member` (operOrPunct !! 0) = Just $ TOperOrPunct [a] 0
    | otherwise  = error $ "getTokenOpOrPunct: error -> " ++ (show $ a:[b])
getTokenOpOrPunct (a:_) _
    | ([a])       `S.member` (operOrPunct !! 0) = Just $ TOperOrPunct [a] 0
    | otherwise  = error $ "getTokenOpOrPunct: error -> " ++ (show $ [a])


getLiteral :: Char -> Char -> Bool -> String -> String
getLiteral _  _  _ []  = []
getLiteral b e False ys@(x : xs)
    | x == b     =  b : getLiteral b e True xs
    | otherwise  = error $ "getLiteral: error near " ++ ys  
getLiteral b e True (x : xs) 
    | x == e     = [e]
    | x == '\\'  = '\\' : x' : getLiteral b e True xs' 
    | otherwise  = x  : getLiteral b e True xs
                    where
                        (x':xs') = xs

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
                        
