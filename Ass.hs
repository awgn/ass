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
import System.FilePath
import System.Directory(getCurrentDirectory)

import Control.Monad(liftM)
import qualified Data.ByteString.Lazy.Char8 as C

import qualified Cpp.Source as Cpp
import qualified Cpp.Filter as Cpp
import qualified Cpp.Token  as Cpp

type Source          = Cpp.Source
type SourceLine      = Cpp.Source

type SourceCode      = [CodeLine]
type TranslationUnit = SourceCode
type MainFunction    = SourceCode
type ParserState     = (TranslationUnit, MainFunction)
 

data Compiler = Gcc | Clang 
                deriving (Show, Eq, Ord)


data CodeLine = CodeLine Int SourceLine 


instance Show CodeLine where
    show (CodeLine n xs) = "#line " ++ show n ++ "\n" ++ C.unpack xs  


snippet, tmpDir :: String 

snippet = "snippet" 
tmpDir =  "/tmp" 

main :: IO Int
main = do args <- getArgs
          cwd' <- getCurrentDirectory
          code <- C.hGetContents stdin
          cxx  <- getCompiler

          let args' = getCompilerArgs args
          let mt    = isMultiThread code args'
          let bin   = tmpDir </> snippet
          let src   = bin <.> "cpp"

          writeSource src (makeSourceCode code mt)

          compileWith cxx src bin mt (["-I", cwd', "-I",  cwd' </> ".."] ++ args') 
                >>= \ec -> if (ec == ExitSuccess)
                           then system (bin ++ " " ++ (unwords $ getTestArgs args)) >>= exitWith
                           else exitFailure


writeSource :: FilePath -> [SourceCode] -> IO ()
writeSource name src = writeFile name (unlines $ map show (concat src))


isMultiThread :: Source -> [String] -> Bool
isMultiThread src xs = "-pthread" `elem` xs  || useThreadOrAsync src 


useThreadOrAsync :: Source -> Bool
useThreadOrAsync src =  "thread" `elem` identifiers || "async" `elem` identifiers   
                            where tokens = filter Cpp.isIdentifier $ Cpp.tokens $ sourceFilter src
                                  identifiers = Cpp.toString <$> tokens


makeSourceCode :: Source -> Bool -> [SourceCode]
makeSourceCode src mt | hasMain src = [ headers, toSourceCode src ]
                      | otherwise   = [ headers, global, mainHeader, body, mainFooter ]
                        where (global, body) = foldl parseCodeLine ([], []) (toSourceCode src) 
                              headers    = [ CodeLine 1 (C.pack $ "#include " ++ if mt then "<ass-mt.hpp>" else "<ass.hpp>")]
                              mainHeader = [ CodeLine 1 (C.pack "int main(int argc, char *argv[]) { cout << boolalpha;") ]
                              mainFooter = [ CodeLine 1 (C.pack "}") ]


hasMain :: Source -> Bool
hasMain src 
    | ["int", "main", "("] `isInfixOf` (Cpp.toString <$> ts) = True
    | otherwise = False
        where ts = Cpp.tokens $ sourceFilter src


sourceFilter :: Source -> Source
sourceFilter = Cpp.filter Cpp.ContextFilter { Cpp.getCode = True, Cpp.getComment = False, Cpp.getLiteral = False }   


getCompilerArgs :: [String] -> [String]
getCompilerArgs = takeWhile ( /= "--" )  


getTestArgs :: [String] -> [String]
getTestArgs = tail' . dropWhile ( /= "--" ) 
                where tail' [] = []
                      tail' (_:xs) = xs


isPreprocessor :: SourceLine -> Bool
isPreprocessor = C.isPrefixOf (C.pack "#") . C.dropWhile isSpace 


getGlobalLine :: SourceLine -> Maybe SourceLine
getGlobalLine xs 
    | C.pack "|||" `C.isPrefixOf` xs' = Just $ snd $ C.splitAt 3 xs'
    | otherwise = Nothing
        where xs' = C.dropWhile isSpace xs


parseCodeLine :: ParserState -> CodeLine -> ParserState
parseCodeLine (t,m) (CodeLine n x)  
    | isPreprocessor x = (t ++ [CodeLine n x], m)
    | Just x' <- getGlobalLine x = (t ++ [CodeLine n x'], m)
    | otherwise  =  (t, m ++ [CodeLine n x])


toSourceCode :: Source -> SourceCode
toSourceCode src = zipWith CodeLine [1..] (C.lines src)


getCompiler :: IO Compiler
getCompiler =  liftM (isSuffixOf "clang") getProgName >>= 
                    (\b -> if b then return Clang else return Gcc)


getCompilerExe :: Compiler -> String
getCompilerExe Gcc   = "/usr/bin/g++"
getCompilerExe Clang = "/usr/bin/clang++"


getCompilerOpt :: Compiler -> Bool -> [String]
getCompilerOpt Gcc   _  =  [ "-std=c++0x", "-O0", "-D_GLIBCXX_DEBUG", "-Wall", "-Wextra", "-Wno-unused-parameter" ]
getCompilerOpt Clang mt =  [ "-std=c++0x", "-O0", "-D_GLIBCXX_DEBUG", "-Wall", "-include-pch", precomp_header, "-Wextra", "-Wno-unused-parameter" , "-Wno-unneeded-internal-declaration"]
                            where precomp_header | mt = "/usr/local/include/ass-mt.hpp.pch"
                                                 | otherwise = "/usr/local/include/ass.hpp.pch" 


compileWith :: Compiler -> FilePath -> FilePath -> Bool -> [String] -> IO ExitCode
compileWith cxx source binary mt user_opt 
            = do -- print cmd 
                 system $ unwords $ cmd
                    where cmd = [getCompilerExe cxx, source, "-o", binary] 
                                ++ (getCompilerOpt cxx mt) 
                                ++ user_opt 
                                ++ if (mt) then ["-pthread"] else []

