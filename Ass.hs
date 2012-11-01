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

import Control.Monad(forM,liftM,filterM)
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
 

data CodeLine = CodeLine Int SourceLine 

instance Show CodeLine where
    show (CodeLine n xs) = "#line " ++ show n ++ "\n" ++ C.unpack xs  


data CompilerType = Clang | Gcc | Any 
                    deriving (Show,Enum)


next :: CompilerType -> CompilerType
next Any = Clang
next   x = succ x


instance Eq CompilerType where
    Gcc    == Gcc   =  True
    Clang  == Clang =  True
    Any    == Gcc   =  True
    Any    == Clang =  True
    Gcc    == Any   =  True
    Clang  == Any   =  True
    _      == _     =  False


data Compiler = Compiler { getCType :: CompilerType, 
                           getCxxExec :: FilePath } 
                deriving (Show, Eq)


compilerList :: [Compiler]
compilerList = [ 
                 Compiler Clang "/usr/bin/clang++",
                 Compiler Clang "/usr/local/bin/clang++",
                 Compiler Gcc   "/usr/bin/g++",
                 Compiler Gcc   "/usr/local/bin/g++"
               ]


getCompilers :: [Compiler] -> IO [Compiler]
getCompilers = filterM (doesFileExist . getCxxExec) 


getCompilerTypeByName :: IO CompilerType
getCompilerTypeByName =  
    liftM (isSuffixOf "clang") getProgName >>= \v -> 
        if v then return Clang else return Gcc


compFilter :: CompilerType -> [Compiler] -> [Compiler]
compFilter t = filter (\n -> t == getCType n) 


banner, snippet, tmpDir :: String 

banner  = "ASSi, version 1.2. :? for help"
snippet = "snippet" 
tmpDir  =  "/tmp" 
   

main :: IO ()
main = do args  <- getArgs
          ctype <- getCompilerTypeByName
          case args of 
            ("-i":_) -> getCompilers compilerList >>= mainLoop (tail args)
            []       -> getCompilers compilerList >>= (\xs -> return (head $ compFilter ctype xs)) >>= mainFun []  
            _        -> getCompilers compilerList >>= (\xs -> return (head $ compFilter ctype xs)) >>= mainFun args 


data State = State { stateCType   :: CompilerType,
                     statePList   :: [String],
                     stateGlobal  :: [String]}
                     deriving (Show, Eq)


mainLoop :: [String] -> [Compiler] -> IO ()
mainLoop args clist = do
    putStrLn banner
    putStr "Compilers found: "
    mapM_ (\c -> putStr (getCxxExec c ++ " ")) clist
    putStrLn "..."
    home <- getHomeDirectory
    runInputT defaultSettings { historyFile = Just $ home </> ".ass_history" } (loop $ State Clang [] [])
    where
    loop :: State -> InputT IO ()
    loop state = do
        minput <- getInputLine "Ass> "
        case (words <$> minput) of
             Nothing -> return ()
             Just (":r":_) -> outputStrLn "Preprocessor/code clean." >> (loop state{ statePList = [], stateGlobal = [] } )
             Just (":s":_) -> outputStrLn "Preprocessor directives:" >> mapM_ outputStrLn (statePList state) >> loop state
             Just (":g":_) -> outputStrLn "Global code:" >> mapM_ outputStrLn (stateGlobal state) >> loop state
             Just (":c":_) -> getCode >>= \xs -> loop state {stateCode = xs} 
             Just (":x":_) -> do 
                            let ctype = next $ stateCType state
                            outputStrLn $ "Using " ++ show (ctype) ++ " compiler..."
                            loop state { stateCType = ctype }
             
             Just (":q":_)    -> outputStrLn "Leaving ASSi." >> return ()
             Just (":?":_)    -> lift printHelp >> loop state
             Just ("///":xs) -> loop state{ stateGlobal = stateGlobal state ++ [unwords $ "///" : xs] } 
             Just []         -> loop state
             Just input | isPreprocessor (C.pack $ unwords input) -> loop state { statePList = statePList state ++ [unwords input] } 
                        | otherwise -> do 
                        e <- lift $ buildCompileRun (C.pack (
                            unlines (statePList state) ++ unlines (stateGlobal state) ++ unwords input))  
                                (compFilter (stateCType state) clist) (getCompilerArgs args) [] 
                        outputStrLn $ " -> " ++ show e
                        loop state


getCode :: InputT IO [String]
getCode = do 
    line <- getInputLine "code> "
    case line of
         Nothing    -> return []
         Just []    -> getCode
         Just input -> (input :) <$> getCode


mainFun :: [String] -> Compiler -> IO ()
mainFun args cxx = do
    code <- C.hGetContents stdin
    buildCompileRun code [cxx] (getCompilerArgs args) (getTestArgs args) >>= (\xs -> return $ head xs) >>= exitWith


printHelp :: IO ()
printHelp =  putStrLn $ "Commands available from the prompt:\n\n" ++
                        "<statement>                 evaluate/run C++ <statement>\n" ++
                        "  :c                        enter in C++ code mode\n" ++ 
                        "  :r                        reset preprocessor and code\n" ++ 
                        "  :s                        show preprocessor directives\n" ++
                        "  :g                        show global code\n" ++
                        "  :x                        switch compiler\n" ++ 
                        "  :q                        quit\n" ++
                        "  :?                        print this help\n"  


buildCompileRun :: Source -> [Compiler] -> [String] -> [String] -> IO [ExitCode] 
-- buildCompileRun code cxx cargs targs | trace ("buildCompileRun") False = undefined
buildCompileRun code clist cargs targs = do 
    cwd' <- getCurrentDirectory
    let mt  = isMultiThread code cargs
    let bin = tmpDir </> snippet
    let src = bin <.> "cpp"
    writeSource src (makeSourceCode code mt)
    forM clist $ \cxx -> do
        putStr (show (getCType cxx) ++ ":") >> hFlush stdout
        e <- compileWith cxx src (binary bin cxx) mt (["-I", cwd', "-I",  cwd' </> ".."] ++ cargs) 
        if (e == ExitSuccess) 
            then system ((binary bin cxx) ++ " " ++ (unwords $ targs)) 
            else return e
        where binary n c = n ++ "-" ++ show (getCType c)


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
getCompilerOpt Any _    = undefined
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
                                ++ (getCompilerOpt (getCType cxx) mt) 
                                ++ user_opt 
                                ++ if (mt) then ["-pthread"] else []


