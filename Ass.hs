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
import System.Info
import System.Exit
import System.FilePath
import System.Directory(getCurrentDirectory, getHomeDirectory, doesFileExist)

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
 

data CompilerType = Cxx | Gcc | Clang 
                    deriving (Show)

instance Eq CompilerType where
    Gcc    == Gcc   =  True
    Clang  == Clang =  True
    Cxx    == Gcc   =  True
    Cxx    == Clang =  True
    Gcc    == Cxx   =  True
    Clang  == Cxx   =  True
    _      == _     =  False


data Compiler = Compiler { getCxxType :: CompilerType, 
                           getCxxExec :: FilePath } 
                deriving (Show, Eq)


compilerList :: [Compiler]

compilerList = [ 
                 Compiler Clang "/usr/bin/clang++",
                 Compiler Clang "/usr/local/bin/clang++",
                 Compiler Gcc   "/usr/bin/g++",
                 Compiler Gcc   "/usr/local/bin/g++"
               ]

getCompiler :: CompilerType -> [Compiler] -> IO Compiler
getCompiler _ [] = error "C++ compiler not found!"

getCompiler t (x:xs) 
        |   t == (getCxxType x) = do
            r <- doesFileExist (getCxxExec x)
            case r of 
                 True  -> return x
                 False -> getCompiler t xs
        |   otherwise = getCompiler t xs  
   

getDefaultCompilerType :: IO CompilerType
getDefaultCompilerType =  
    liftM (isSuffixOf "clang") getProgName >>= \v -> if v then return Clang else return Gcc


data CodeLine = CodeLine Int SourceLine 


instance Show CodeLine where
    show (CodeLine n xs) = "#line " ++ show n ++ "\n" ++ C.unpack xs  


banner, snippet, tmpDir :: String 

banner  = "ASSi, version 1.2. ? for help"
snippet = "snippet" 
tmpDir  =  "/tmp" 
   

main :: IO ()
main = do args <- getArgs
          def  <- getDefaultCompilerType
          case args of 
            ("-i":_) -> getCompiler Cxx compilerList >>= mainLoop (tail args)
            []       -> getCompiler def compilerList >>= mainFun [] 
            _        -> getCompiler def compilerList >>= mainFun args 

printHelp :: IO ()
printHelp =  putStrLn $ "Commands available from the prompt:\n\n" ++
                        "<statement>                 evaluate/run C++ <statement>\n" ++
                        "  r                         reset preprocessor and code\n" ++ 
                        "  s                         show preprocessor directives\n" ++
                        "  g                         show global code\n" ++
                        "  x                         switch compiler\n" ++ 
                        "  q                         quit\n" ++
                        "  ?                         print this help\n"  


data State = State { stateComp   :: Compiler,
                     statePList  :: [String],
                     stateGlobal :: [String]}
                     deriving (Show, Eq)


mainLoop :: [String] -> Compiler -> IO ()
mainLoop args cxx = do
    putStrLn (banner ++ "\nUsing " ++ getCxxExec cxx ++ " compiler...") 
    home <- getHomeDirectory
    runInputT defaultSettings { historyFile = Just $ home </> ".ass_history" } (loop $ State cxx [] [])
    where
    loop :: State -> InputT IO ()
    loop state = do
        minput <- getInputLine "Ass> "
        case (words <$> minput) of
             Nothing -> return ()
             Just ("r":_) -> outputStrLn "Preprocessor and code clean." >> (loop state{ statePList = [], stateGlobal = [] } )
             Just ("s":_) -> outputStrLn "Preprocessor directives:" >> mapM_ outputStrLn (statePList state) >> loop state
             Just ("g":_) -> outputStrLn "Global code:" >> mapM_ outputStrLn (stateGlobal state) >> loop state
             Just ("x":_) -> do 
                         cxx' <- lift $ getCompiler (if (getCxxType $ stateComp state) == Gcc then Clang else Gcc) compilerList 
                         outputStrLn $ "Using " ++ getCxxExec cxx' ++ " compiler..."
                         loop state{ stateComp = cxx' }
             Just ("q":_) -> outputStrLn "Leaving ASSi." >> return ()
             Just ("?":_) -> lift printHelp >> loop state
             Just ("///":xs) -> loop state{ stateGlobal = stateGlobal state ++ [unwords xs] } 
             Just []      -> loop state
             Just input | isPreprocessor (C.pack $ unwords input) -> loop state { statePList = statePList state ++ [unwords input] } 
                        | otherwise -> do 
                        e <- lift $ buildCompileRun (C.pack (
                            unlines (statePList state) ++ unlines (stateGlobal state) ++ unwords input))  
                                (stateComp state) (getCompilerArgs args) [] 
                        outputStrLn $ " -> " ++ show e
                        loop state


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
    | C.pack "///" `C.isPrefixOf` xs' = Just $ snd $ C.splitAt 3 xs'
    | otherwise = Nothing
        where xs' = C.dropWhile isSpace xs


parseCodeLine :: ParserState -> CodeLine -> ParserState
parseCodeLine (t,m) (CodeLine n x)  
    | isPreprocessor x = (t ++ [CodeLine n x], m)
    | Just x' <- getGlobalLine x = (t ++ [CodeLine n x'], m)
    | otherwise  =  (t, m ++ [CodeLine n x])


toSourceCode :: Source -> SourceCode
toSourceCode src = zipWith CodeLine [1..] (C.lines src)


getCompilerOpt :: CompilerType -> Bool -> [String]
getCompilerOpt Cxx _    = undefined
getCompilerOpt Gcc _    =  [ "-std=c++0x", "-O0", "-D_GLIBCXX_DEBUG", "-Wall", 
                             "-Wextra", "-Wno-unused-parameter" ]
getCompilerOpt Clang mt =  compilerLib ++ [ "-std=c++0x", "-O0", "-D_GLIBCXX_DEBUG", "-Wall", 
                             "-include-pch", precomp_header, "-Wextra", "-Wno-unused-parameter" , 
                             "-Wno-unneeded-internal-declaration"]
                           where precomp_header | mt             = "/usr/local/include/ass-mt.hpp.pch"
                                                | otherwise      = "/usr/local/include/ass.hpp.pch" 
                                 compilerLib    | os == "darwin" = [ "-stdlib=libc++" ]
                                                | otherwise      = []

compileWith :: Compiler -> FilePath -> FilePath -> Bool -> [String] -> IO ExitCode
compileWith cxx source binary mt user_opt 
            = do system $ unwords $ cmd
                    where cmd = [getCxxExec cxx, source, "-o", binary] 
                                ++ (getCompilerOpt (getCxxType cxx) mt) 
                                ++ user_opt 
                                ++ if (mt) then ["-pthread"] else []


