--
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
-- ass: C++ code ass'istant

import Char
import System(getArgs)
import System.Process
import System.IO
import System.Exit
import System.Directory

import Compiler

data CodeLine = CodeLine Int String 

instance Show CodeLine where
    show (CodeLine n s) = "#line " ++ show n ++ "\n" ++ s  

type SourceCode      = [CodeLine]

type TranslationUnit = SourceCode
type MainFunction    = SourceCode

type ParserState = (TranslationUnit, MainFunction)

main :: IO Int
main = do
    args <- getArgs
    cwd' <- getCurrentDirectory

    let mainHeader = [ CodeLine 0 "#include <ass.hpp>" ]
    let mainBegin  = [ CodeLine 0 "int main(int argc, char *argv[]) { cout << boolalpha;" ]
    let mainEnd    = [ CodeLine 0 "}" ]
    let testCmd    = "/tmp/runme " ++ ( unwords $ getTestArgs args ) 

    -- parse the snippet.
    snippet <- hGetContents stdin >>= toCode

    let (translationUnit,mainBody) = parseSourceCode (mainHeader,[]) snippet

    -- create source code.
    handle <- openFile "/tmp/runme.cpp" WriteMode
    mapM_ (hPrint handle) translationUnit
    mapM_ (hPrint handle) mainBegin
    mapM_ (hPrint handle) mainBody
    mapM_ (hPrint handle) mainEnd
    hClose handle
    
    -- compile and run it.
    ec <- compileWith Gxx "/tmp/runme.cpp" "/tmp/runme" (("-I " ++ cwd'):(getCompilerArgs args))  
    if (ec == ExitSuccess)  
        then do 
            system testCmd >>= exitWith
        else exitWith $ ExitFailure 1


getCompilerArgs :: [String] -> [String]
getCompilerArgs = takeWhile ( /= "--" )  

getTestArgs :: [String] -> [String]
getTestArgs = tail' . dropWhile ( /= "--" ) 

tail' :: [String] -> [String]
tail' [] = []
tail' (_:xs) = xs


isPreprocessor :: String -> Bool
isPreprocessor [] = False
isPreprocessor (x:xs) 
   | isSpace x = isPreprocessor xs
   | x == '#'  = True
   | otherwise = False


asGlobal :: String -> Maybe String
asGlobal [] = Nothing
asGlobal (x:xs)
    | isSpace x = getGlobal xs
    | x == '$' =  Just xs
    | otherwise = Nothing


parseSourceCode :: ParserState -> SourceCode -> ParserState
parseSourceCode s [] = s
parseSourceCode s (x:xs) = parseSourceCode (parseCodeLine s x) xs


parseCodeLine :: ParserState -> CodeLine -> ParserState
parseCodeLine (t,m) (CodeLine n x) 
    | isPreprocessor x = (t ++ [CodeLine n x], m)
    | Just x' <- asGlobal x = (t ++ [CodeLine n x'], m)
    | otherwise  =  (t, m ++ [CodeLine n x])


toCode :: String -> IO SourceCode
toCode l = return (map (\ (xs,n) -> CodeLine n xs) $ zip (lines l) [1..])

