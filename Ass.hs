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
-- ass: C++11 code ass'istant for vim


import Data.Char
import Data.List
import System(getArgs)
import System.Process
import System.IO
import System.Exit
import System.Directory

import qualified CppFilter as CF
import qualified CppToken  as CT


data CodeLine = CodeLine Int String 


instance Show CodeLine where
    show (CodeLine n xs) = "#line " ++ show n ++ "\n" ++ xs  


type SourceCode      = [CodeLine]
type TranslationUnit = SourceCode
type MainFunction    = SourceCode
type ParserState     = (TranslationUnit, MainFunction)


main :: IO Int
main = do
    
    args <- getArgs
    cwd' <- getCurrentDirectory
    src  <- hGetContents stdin

    writeSource "/tmp/snippet.cpp" $ makeSourceCode src
    
    ec <- compileWith "/usr/bin/g++" "/tmp/snippet.cpp" "/tmp/snippet" $ ("-I " ++ cwd'):("-I " ++ cwd' ++ "/.."):(getCompilerArgs args)  
    if (ec == ExitSuccess)  
    then 
        system ("/tmp/snippet " ++ (unwords $ getTestArgs args)) >>= exitWith
    else 
        exitWith $ ExitFailure 1


makeSourceCode :: String -> [ SourceCode ]
makeSourceCode xs 
    | isSnippet xs = [ mainHeader, toSourceCode xs ]
    | otherwise    = composeSrc $ foldl parseCodeLine (mainHeader, []) $ toSourceCode xs
        where mainHeader = [ CodeLine 0 "#include <ass.hpp>" ]
              mainBegin  = [ CodeLine 0 "int main(int argc, char *argv[]) { cout << boolalpha;" ]
              mainEnd    = [ CodeLine 0 "}" ]
              composeSrc (global,body) = [global, mainBegin, body, mainEnd]


isSnippet :: String -> Bool
isSnippet xs 
    | ["int", "main", "("] `isInfixOf` (map CT.toString $ CT.tokens $ sourceFilter xs) = True
    | otherwise = False


sourceFilter :: String -> String
sourceFilter xs = CF.runSourceFilter xs (True, False, False)   


writeSource :: String -> [ SourceCode ] -> IO ()
writeSource name xs = writeFile name (intercalate "\n" $ map show (concat xs))


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
compileWith comp source outfile opts = system (
                 unwords $ [ comp, source, "-o", outfile ] 
                 ++ [ "-std=c++0x", "-O0", "-D_GLIBCXX_DEBUG", "-Wall", "-Wextra", "-Wno-unused-parameter" ]  
                 ++ opts ) 


