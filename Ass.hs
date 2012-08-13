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


module Main where


import Data.Char
import Data.List
import Data.Functor
import System.Environment(getArgs, getProgName)
import System.Process(system)
import System.IO
import System.Exit
import System.Directory(getCurrentDirectory)
import Control.Monad(liftM)

import qualified CppFilter as F
import qualified CppToken  as T


type Source          = String
type Binary          = String
type SourceCode      = [CodeLine]
type TranslationUnit = SourceCode
type MainFunction    = SourceCode
type ParserState     = (TranslationUnit, MainFunction)
 
data Compiler = Gcc | Clang 
                deriving (Show, Eq, Ord)

data CodeLine = CodeLine Int String 

instance Show CodeLine where
    show (CodeLine n xs) = "#line " ++ show n ++ "\n" ++ xs  


main :: IO Int
main = do args <- getArgs
          cwd' <- getCurrentDirectory
          src  <- hGetContents stdin
          comp <- getCompiler

          let cargs = getCompilerArgs args
          let mt    = isMultiThread src cargs

          writeSource "/tmp/snippet.cpp" $ makeSourceCode src mt

          ec <- compileWith comp "/tmp/snippet.cpp" "/tmp/snippet" mt $ ["-I", cwd', "-I",  cwd' ++ "/.."] ++ cargs 

          if (ec == ExitSuccess)  
          then system ("/tmp/snippet " ++ (unwords $ getTestArgs args)) >>= exitWith
          else exitWith $ ExitFailure 1


isMultiThread :: Source -> [String] -> Bool
isMultiThread src xs = "-pthread" `elem` xs  || useThreadOrAsync src 


useThreadOrAsync :: Source -> Bool
useThreadOrAsync src =  "thread" `elem` identifiers || "async" `elem` identifiers   
                            where tokens = filter T.isTIdentifier $ T.tokens $ sourceFilter src
                                  identifiers = T.toString <$> tokens


makeSourceCode :: Source -> Bool -> [ SourceCode ]
makeSourceCode src mt | hasMain src = [ headers, toSourceCode src ]
                      | otherwise   = [ headers, global, mainHeader, body, mainFooter ]
                        where (global, body) = foldl parseCodeLine ([], []) (toSourceCode src) 
                              headers    = [ CodeLine 1 ("#include " ++ if mt then "<ass-mt.hpp>" else "<ass.hpp>")]
                              mainHeader = [ CodeLine 1 "int main(int argc, char *argv[]) { cout << boolalpha;" ]
                              mainFooter = [ CodeLine 1 "}" ]


hasMain :: Source -> Bool
hasMain src 
    | ["int", "main", "("] `isInfixOf` (T.toString <$> ts) = True
    | otherwise = False
        where ts = T.tokens $ sourceFilter src


sourceFilter :: Source -> Source
sourceFilter = F.cppFilter (True, False, False)   


writeSource :: FilePath -> [ SourceCode ] -> IO ()
writeSource name src = writeFile name (unlines $ map show (concat src))


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
    | "|||" `isPrefixOf` xs' = Just $ snd $ splitAt 3 xs'
    | otherwise = Nothing
        where xs' = dropWhile isSpace xs


parseCodeLine :: ParserState -> CodeLine -> ParserState
parseCodeLine (t,m) (CodeLine n x)  
    | isPreprocessor x = (t ++ [CodeLine n x], m)
    | Just x' <- getGlobalLine x = (t ++ [CodeLine n x'], m)
    | otherwise  =  (t, m ++ [CodeLine n x])


toSourceCode :: Source -> SourceCode
toSourceCode src = zipWith CodeLine [1..] (lines src)


getCompiler :: IO Compiler
getCompiler = liftM (isSuffixOf "clang") getProgName >>= 
                    (\b -> if b then (return Clang) else (return Gcc))


getCompilerExe :: Compiler -> String
getCompilerExe Gcc   = "/usr/bin/g++"
getCompilerExe Clang = "/usr/bin/clang++"


getCompilerOpt :: Compiler -> Bool -> [String]
getCompilerOpt Gcc   _  =  [ "-std=c++0x", "-O0", "-D_GLIBCXX_DEBUG", "-Wall", "-Wextra", "-Wno-unused-parameter" ]
getCompilerOpt Clang mt =  [ "-std=c++0x", "-O0", "-D_GLIBCXX_DEBUG", "-Wall", "-include-pch", precomp_header, "-Wextra", "-Wno-unused-parameter" , "-Wno-unneeded-internal-declaration"]
                            where precomp_header | mt = "/usr/local/include/ass-mt.hpp.pch"
                                                 |otherwise = "/usr/local/include/ass.hpp.pch" 


compileWith :: Compiler -> Source -> Binary -> Bool -> [String] -> IO ExitCode
compileWith comp source binary mt user_opt 
            = do -- print cmd 
                 system $ unwords $ cmd
                    where cmd = [getCompilerExe comp, source, "-o", binary] 
                                ++ (getCompilerOpt comp mt) 
                                ++ user_opt 
                                ++ if (mt) then ["-pthread"] else []

