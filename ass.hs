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

data CodeLine = CodeLine Int String 

instance Show CodeLine where
    show (CodeLine n s) = "#line " ++ show n ++ "\n" ++ s  

type SourceCode      = [CodeLine]

type TranslationUnit = SourceCode
type MainFunction    = SourceCode
type Section         = Bool


isPreprocessor :: String -> Bool
isPreprocessor [] = False
isPreprocessor (x:xs) 
   | isSpace x = isPreprocessor xs
   | x == '#'  = True
   | otherwise = False

isSeparator :: String -> Bool
isSeparator xs = (spaces + dots) == (length xs) && (dots > 2)
            where
                spaces = length $ filter isSpace xs
                dots   = length $ filter ( == '.') xs 

getCompilerArgs :: [String] -> [String]
getCompilerArgs = takeWhile ( /= "--" )  

tail' :: [String] -> [String]
tail' [] = []
tail' (_:xs) = xs

getTestArgs :: [String] -> [String]
getTestArgs = tail' . dropWhile ( /= "--" ) 

type ParserState = (TranslationUnit, MainFunction, Section)

parseCodeLine :: ParserState -> CodeLine -> ParserState
parseCodeLine (t,m,s) (CodeLine n x) 
    | isSeparator x     = (t, m, not s)
    | isPreprocessor x  = ((t ++ [CodeLine n x]), m, s)
    | otherwise         = if s 
                            then ((t ++ [CodeLine n x]), m, s)
                            else (t, (m ++ [CodeLine n x]), s)

parseSourceCode :: ParserState -> SourceCode -> ParserState
parseSourceCode s [] = s
parseSourceCode s (x:xs) = parseSourceCode (parseCodeLine s x) xs

toCode :: String -> IO SourceCode
toCode l = return (map (\ (xs,n) -> CodeLine n xs) $ zip (lines l) [1..])

main :: IO Int
main = do
    args <- getArgs
    
    let mainHeader = [ CodeLine 0 "#include <ass.hpp>" ]
    let mainBegin  = [ CodeLine 0 "int main(int argc, char *argv[]) { cout << boolalpha;" ]
    let mainEnd    = [ CodeLine 0 "}" ]

    let compileCmd = "/usr/bin/g++ -std=c++0x -Wall -Wextra -Wno-unused-parameter " 
                        ++ "-D_GLIBXX_DEBUG /tmp/runme.cpp -o /tmp/runme "  ++ ( unwords $ getCompilerArgs args )
    let testCmd = "/tmp/runme " ++ ( unwords $ getTestArgs args ) 

    -- parse the snippet.
    snippet <- hGetContents stdin >>= toCode

    let (translationUnit,mainBody,_) = parseSourceCode (mainHeader,[],False) snippet

    -- create source code.
    handle <- openFile "/tmp/runme.cpp" WriteMode
    mapM_ (hPrint handle) translationUnit
    mapM_ (hPrint handle) mainBegin
    mapM_ (hPrint handle) mainBody
    mapM_ (hPrint handle) mainEnd
    hClose handle
    
    -- compile and run it.
    runCommand compileCmd >>= waitForProcess >> runCommand testCmd >>= waitForProcess >>= exitWith

