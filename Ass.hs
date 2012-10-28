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
import System.Directory(getCurrentDirectory, doesFileExist)

import System.Console.Haskeline

import Control.Monad(liftM)
import Control.Monad.Trans.Class

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
 

data Compiler = Gcc { getExec :: FilePath } | Clang { getExec :: FilePath }
                deriving (Show, Eq)

isClang :: Compiler -> Bool
isClang (Gcc   _) = False
isClang (Clang _) = True

isGcc :: Compiler -> Bool
isGcc (Gcc   _) = True
isGcc (Clang _) = False


compilerList :: [Compiler]

compilerList = [ 
                 Clang "/usr/bin/clang++",
                 Clang "/usr/local/bin/clang++",
                 Gcc   "/usr/bin/g++",
                 Gcc   "/usr/local/bin/g++"
               ]

getDefaultCompiler :: [Compiler] -> IO Compiler

getDefaultCompiler [] = error "C++ compiler not found!"
getDefaultCompiler (x:xs) = do 
           r <- doesFileExist (getExec x)
           case r of 
                True  -> return x
                False -> getDefaultCompiler xs                                                         


getCompiler :: [Compiler] -> IO Compiler
getCompiler list =  liftM (isSuffixOf "clang") getProgName >>= \clang ->
                    if clang  
                    then getDefaultCompiler (filter isClang list)  
                    else getDefaultCompiler (filter isGcc   list)


data CodeLine = CodeLine Int SourceLine 


instance Show CodeLine where
    show (CodeLine n xs) = "#line " ++ show n ++ "\n" ++ C.unpack xs  


banner, snippet, tmpDir :: String 

banner  = "ASSi, version 1.1. ? for help"
snippet = "snippet" 
tmpDir  =  "/tmp" 
   

main :: IO ()
main = do args <- getArgs
          case args of 
            ("-i":_) -> getDefaultCompiler compilerList >>= mainLoop (tail args)
            []       -> getCompiler compilerList >>= mainFun [] 
            _        -> getCompiler compilerList >>= mainFun args 

printHelp :: IO ()
printHelp =  putStrLn $ "Commands available from the prompt:\n\n" ++
                        "<statement>                 evaluate/run C++ <statement>\n" ++
                        "  c                         clear preprocessor list\n" ++ 
                        "  q                         quit\n" ++
                        "  ?                         print this help\n"  


mainLoop :: [String] -> Compiler -> IO ()
mainLoop args cxx = putStrLn (banner ++ "\nUsing " ++ getExec cxx ++ " compiler.") >> runInputT defaultSettings (loop [])
   where
       loop :: [String] -> InputT IO ()
       loop ppList = do
           minput <- getInputLine "Ass> "
           case minput of
               Nothing -> return ()
               Just "c"    -> outputStrLn "Preprocessor directive clean." >> (loop [])
               Just "q"    -> outputStrLn "Leaving ASSi." >> return ()
               Just "?"    -> lift printHelp >> (loop ppList)
               Just ""     -> loop ppList
               Just input | isPreprocessor (C.pack input) -> loop $ input : ppList 
                          | otherwise -> do 
                              e <- lift $ buildCompileRun (C.pack (unlines $ ppList ++ [input])) cxx (getCompilerArgs args) [] 
                              outputStrLn $ show e
                              loop ppList


mainFun :: [String] -> Compiler -> IO ()
mainFun args cxx = do
    code <- C.hGetContents stdin
    buildCompileRun code cxx (getCompilerArgs args) (getTestArgs args) >>= exitWith


buildCompileRun :: Source -> Compiler -> [String] -> [String] -> IO ExitCode 
-- buildCompileRun code cxx cargs targs | trace ("buildCompileRun") False = undefined
buildCompileRun code cxx cargs targs = do 
    cwd' <- getCurrentDirectory
    let mt  = isMultiThread code cargs
    let bin = tmpDir </> snippet
    let src = bin <.> "cpp"
    writeSource src (makeSourceCode code mt)
    ec <- compileWith cxx src bin mt (["-I", cwd', "-I",  cwd' </> ".."] ++ cargs) 
    if (ec == ExitSuccess) 
       then system (bin ++ " " ++ (unwords $ targs)) 
       else return ec


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


getCompilerOpt :: Compiler -> Bool -> [String]
getCompilerOpt (Gcc _)   _  =  [ "-std=c++0x", "-O0", "-D_GLIBCXX_DEBUG", "-Wall", 
                                 "-Wextra", "-Wno-unused-parameter" ]
getCompilerOpt (Clang _) mt =  [ "-std=c++0x", "-O0", "-D_GLIBCXX_DEBUG", "-Wall", 
                                 "-include-pch", precomp_header, "-Wextra", 
                                 "-Wno-unused-parameter" , "-Wno-unneeded-internal-declaration"]
                            where precomp_header | mt = "/usr/local/include/ass-mt.hpp.pch"
                                                 | otherwise = "/usr/local/include/ass.hpp.pch" 


compileWith :: Compiler -> FilePath -> FilePath -> Bool -> [String] -> IO ExitCode
compileWith cxx source binary mt user_opt 
            = do system $ unwords $ cmd
                    where cmd = [getExec cxx, source, "-o", binary] 
                                ++ (getCompilerOpt cxx mt) 
                                ++ user_opt 
                                ++ if (mt) then ["-pthread"] else []


