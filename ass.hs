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
-- ass: C++ code ass'istant for vim

import Data.Char
import Data.List
import System(getArgs)
import System.Process
import System.IO
import System.Exit
import System.Directory
import Control.Monad

data CodeLine = CodeLine Int String 

instance Show CodeLine where
    show (CodeLine n xs) = "#line " ++ show n ++ "\n" ++ xs  

type SourceCode      = [CodeLine]
type TranslationUnit = SourceCode
type MainFunction    = SourceCode

type ParserState = (TranslationUnit, MainFunction)

main :: IO Int
main = do
    
    args <- getArgs
    cwd' <- getCurrentDirectory

    let testCmd = "/tmp/snippet " ++ (unwords $ getTestArgs args)  

    -- get the snippet.

    src <- hGetContents stdin

    -- build the source code...

    buildSource "/tmp/snippet.cpp" $ getSourceCode src
    
    -- compile and run it.

    ec <- compileWith "/usr/bin/g++" "/tmp/snippet.cpp" "/tmp/snippet" $ ("-I " ++ cwd'):(getCompilerArgs args)  
    if (ec == ExitSuccess)  
    then do 
        system testCmd >>= exitWith
    else exitWith $ ExitFailure 1


getSourceCode :: String -> [ SourceCode ]
getSourceCode xs | isSnippet xs = [ toSourceCode xs ]
                 | otherwise    = composeSrc $ foldl parseCodeLine (mainHeader, []) $ toSourceCode xs
                 where mainHeader = [ CodeLine 0 "#include <ass.hpp>" ]
                       mainBegin  = [ CodeLine 0 "int main(int argc, char *argv[]) { cout << boolalpha;" ]
                       mainEnd    = [ CodeLine 0 "}" ]
                       composeSrc (global,body) = [global, mainBegin, body, mainEnd]


isSnippet :: String -> Bool
isSnippet xs = ["int", "main"] `isInfixOf` (words $ map (\x -> if isAlphaNum(x) then x else ' ') xs)


buildSource :: String -> [ SourceCode ] -> IO ()
buildSource name xs = do
                      h <- openFile name WriteMode 
                      _ <- forM xs $ (\ys -> mapM (hPrint h) ys)
                      hClose h
            

getCompilerArgs :: [String] -> [String]
getCompilerArgs = takeWhile ( /= "--" )  


getTestArgs :: [String] -> [String]
getTestArgs = tail' . dropWhile ( /= "--" ) 
                where tail' [] = []
                      tail' (_:xs) = xs


isPreprocessor :: String -> Bool
isPreprocessor = isPrefixOf "#" . dropWhile isSpace 



getGlobalLine :: String -> Maybe String
getGlobalLine xs 
    | "..." `isPrefixOf` xs' = Just $ tail $ tail $ tail xs'
    | otherwise = Nothing
    where xs' = dropWhile isSpace xs


parseCodeLine :: ParserState -> CodeLine -> ParserState
parseCodeLine (t,m) (CodeLine n x)  
    | isPreprocessor x = (t ++ [CodeLine n x], m)
    | Just x' <- getGlobalLine x = (t ++ [CodeLine n x'], m)
    | otherwise  =  (t, m ++ [CodeLine n x])


toSourceCode :: String -> SourceCode
toSourceCode xs = zipWith CodeLine [1..] (lines xs)


compileWith :: String -> String -> String -> [String] -> IO ExitCode
compileWith comp source out extra = system (
                            unwords $ [ comp, source, "-o", out ] 
                            ++ [ "-std=c++0x", "-O0", "-D_GLIBXX_DEBUG", "-Wall", "-Wextra", "-Wno-unused-parameter" ]  
                            ++ extra ) 

