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

{-# LANGUAGE OverloadedStrings #-} 

module Main where

import Data.Char
import Data.List
import Data.Functor
import Safe (tailSafe)
import System.Environment(getArgs, getProgName)
import System.Process(system)
import System.IO
-- import System.Info
import System.Exit
import System.FilePath
import System.Directory(getCurrentDirectory, getHomeDirectory, doesFileExist)

import System.Console.Haskeline

import Control.Monad(when,void,forM,liftM,filterM)
import Control.Monad.Trans.Class

import qualified Data.ByteString.Char8 as C

import qualified Cpp.Source as Cpp
import qualified Cpp.Filter as Cpp
import qualified Cpp.Token  as Cpp

type Source          = Cpp.Source
type SourceLine      = Cpp.Source

type SourceCode      = [CodeLine]
type TranslationUnit = SourceCode
type MainFunction    = SourceCode
type ParserState     = (TranslationUnit, MainFunction)
 

-- default compiler list (overridden by ~/.assrc)
--

compilerList :: [Compiler]
compilerList = [ 
                 Compiler Gcc48   "/usr/bin/g++-4.8" "g++-4.8" [],
                 Compiler Gcc47   "/usr/bin/g++-4.7" "g++-4.7" [],
                 Compiler Gcc46   "/usr/bin/g++-4.6" "g++-4.6" [],
                 Compiler Clang33 "/usr/bin/clang++" "clang++" []
               ]


banner, snippet, tmpDir,assrc, ass_history :: String 

banner  = "ASSi, version 1.2.5 :? for help"
snippet     = "snippet" 
tmpDir      =  "/tmp" 
assrc       =  ".assrc"
ass_history = ".ass_history"


data CodeLine = CodeLine Int SourceLine 


instance Show CodeLine where
    show (CodeLine n xs) = "#line " ++ show n ++ "\n" ++ C.unpack xs  

-- Compiler:

data CompilerType = Gcc46 | Gcc47 | Gcc48 | Clang31 | Clang32 | Clang33 
                    deriving (Eq,Show,Read,Enum)

next :: CompilerType -> CompilerType
next Clang33 = Gcc46
next   x = succ x


data CompilerFamily = Gcc | Clang 
                    deriving (Eq,Show,Read,Enum)


data Compiler = Compiler CompilerType FilePath String [String] 
                deriving (Read, Eq)


instance Show Compiler where
    show (Compiler _ _ name opt) = name ++ " (" ++ show opt ++ ")"


getCompilerType :: Compiler -> CompilerType 
getCompilerType (Compiler t _ _ _) = t


getCompilerFamily :: Compiler -> CompilerFamily
getCompilerFamily (Compiler Clang31 _ _ _) = Clang
getCompilerFamily (Compiler Clang32 _ _ _) = Clang
getCompilerFamily (Compiler Clang33 _ _ _) = Clang
getCompilerFamily _ = Gcc


getCompilerExec :: Compiler -> FilePath
getCompilerExec (Compiler _ e _ _) = e


getCompilerName :: Compiler -> FilePath
getCompilerName (Compiler _ _ n _) = n


getCompilerExtraOpt :: Compiler -> [String]
getCompilerExtraOpt (Compiler _ _ _ xs) = xs


getCompilers :: [Compiler] -> IO [Compiler]
getCompilers = filterM (doesFileExist . getCompilerExec) 


getCompilerFamilyByName :: IO CompilerFamily
getCompilerFamilyByName =  
    liftM (isSuffixOf "clang") getProgName >>= \v -> 
        return $ if v then Clang else Gcc


compFilter :: CompilerFamily -> [Compiler] -> [Compiler]
compFilter t = filter (\c -> t == getCompilerFamily c) 


compFilterType :: CompilerType -> [Compiler] -> [Compiler]
compFilterType t = filter (\c -> t == getCompilerType c) 


getCompilerConf :: FilePath -> IO [Compiler]
getCompilerConf conf = 
    doesFileExist conf >>= \b -> 
        if b then liftM read $ readFile conf
             else return compilerList

main :: IO ()
main = do args  <- getArgs
          home  <- getHomeDirectory
          cfamily <- getCompilerFamilyByName
          clist <- getCompilerConf (home </> assrc)
          case args of 
            ("-i":_) -> getCompilers clist >>= mainLoop (tail args)
            []       -> liftM (head . compFilter cfamily) (getCompilers clist) >>= mainFun []  
            _        -> liftM (head . compFilter cfamily) (getCompilers clist) >>= mainFun args 


data CliState = CliState { stateCType   :: CompilerType,
                           statePList   :: [String],
                           stateCode    :: [String]} deriving (Show, Eq)


mainLoop :: [String] -> [Compiler] -> IO ()
mainLoop args clist = do
    putStrLn banner
    putStr "Compilers found: "
    mapM_ (\c -> putStr (getCompilerExec c ++ " ")) clist
    putChar '\n'
    home <- getHomeDirectory
    runInputT defaultSettings { historyFile = Just $ home </> ".ass_history" } (loop True $ CliState (getCompilerType $ head clist) [] [])
    where
    loop :: Bool -> CliState -> InputT IO ()
    loop ban state = 
        
        if null $ compFilterType (stateCType state) clist 
        then loop ban state { stateCType = next (stateCType state) }  
        else do 
            when ban $ outputStrLn $ "Using " ++ show (stateCType state) ++ " compiler..."
            minput <- getInputLine "Ass> "
            case words <$> minput of
                 Nothing -> return ()
                 Just (":r":_) -> outputStrLn "Code clean." >> loop True state{ statePList = [], stateCode = [] } 
                 Just (":s":_) -> outputStrLn "C++ Code:" >> 
                                  mapM_ outputStrLn (statePList state) >> 
                                  mapM_ outputStrLn (stateCode state) >> loop False state
                 Just (":c":_)    -> getCode    >>= \xs -> loop False state {stateCode = stateCode state ++ xs } 
                 Just (":l":f:[]) -> outputStrLn (f ++ " loaded.") >> loadCode f >>= \xs -> loop False state {stateCode = xs }
                 Just (":q":_) -> void (outputStrLn "Leaving ASSi.")
                 Just (":?":_) -> lift printHelp >> loop True state
                 Just (":n":_) -> loop True state { stateCType = next (stateCType state) }
                 Just []       -> loop False state
                 Just input | isPreprocessor (C.pack $ unwords input) -> loop False state { statePList = statePList state ++ [unwords input] } 
                            | otherwise -> do 
                            e <- lift $ buildCompileAndRun (C.pack(
                                unlines (statePList state) ++ unlines (stateCode state))) (C.pack (unwords input)) True  
                                    (compFilterType (stateCType state) clist) (getCompilerArgs args) [] 
                            outputStrLn $ show e
                            loop False state


getCode :: InputT IO [String]
getCode = do 
    line <- getInputLine "code> "
    case line of
         Nothing    -> return []
         Just []    -> getCode
         Just input -> (input :) <$> getCode


loadCode :: FilePath -> InputT IO [String]
loadCode f = lift $ lines <$> readFile f

mainFun :: [String] -> Compiler -> IO ()
mainFun args cxx = do
    code <- C.hGetContents stdin
    liftM head
        (buildCompileAndRun "" code False [cxx] (getCompilerArgs args) (getTestArgs args)) >>= exitWith


printHelp :: IO ()
printHelp =  putStrLn $ "Commands available from the prompt:\n\n" ++
                        "<statement>                 evaluate/run C++ <statement>\n" ++
                        "  :c                        enter in C++ code mode\n" ++ 
                        "  :l file                   load file in C++ code mode\n" ++ 
                        "  :s                        show code\n" ++
                        "  :r                        reset preprocessor/code\n" ++ 
                        "  :n                        switch to next compiler(s)\n" ++ 
                        "  :q                        quit\n" ++
                        "  :?                        print this help\n"  


buildCompileAndRun :: Source -> Source -> Bool -> [Compiler] -> [String] -> [String] -> IO [ExitCode] 
-- buildCompileRun code' code inter cxx cargs targs | trace ("buildCompileRun") False = undefined
buildCompileAndRun code' code inter clist cargs targs = do 
    cwd' <- getCurrentDirectory
    let mt = isMultiThread code cargs
    let bin = tmpDir </> snippet
    let src = bin <.> "cpp"
    writeSource src (makeSourceCode code' code inter mt)
    forM clist $ \cxx -> do
        when (length clist > 1) $ putStr (show cxx ++ " -> ") >> hFlush stdout
        e <- compileWith cxx src (binary bin cxx) mt (["-I", cwd', "-I",  cwd' </> ".."] ++ cargs) 
        if e == ExitSuccess 
            then system (binary bin cxx ++ " " ++ unwords targs) >>= (\ret -> putChar '\n' >> return ret)
            else return e
        where binary n c = n ++ "-" ++ show (getCompilerType c)


writeSource :: FilePath -> [SourceCode] -> IO ()
writeSource name src = writeFile name (unlines $ map show (concat src))


isMultiThread :: Source -> [String] -> Bool
isMultiThread src xs = "-pthread" `elem` xs  || useThreadOrAsync src 


useThreadOrAsync :: Source -> Bool
useThreadOrAsync src =  "thread" `elem` identifiers || "async" `elem` identifiers   
                            where tokens = filter Cpp.isIdentifier $ Cpp.tokenizer $ sourceFilter src
                                  identifiers = Cpp.toString <$> tokens


makeSourceCode :: Source -> Source -> Bool -> Bool -> [SourceCode]
makeSourceCode src' src lambda mt 
    | hasMain src = [ headers, toSourceCode src', toSourceCode src ]
    | otherwise   = [ headers, toSourceCode src', global, mainHeader, body, mainFooter ]
      where (global, body) = foldl parseCodeLine ([], []) (toSourceCode src) 
            headers    = [ CodeLine 1 include]
            mainHeader = if lambda 
                            then [ CodeLine 1 "int main(int argc, char *argv[]) { cout << boolalpha; ass::cmdline([] {" ] 
                            else [ CodeLine 1 "int main(int argc, char *argv[]) { cout << boolalpha;" ]
            mainFooter = if lambda 
                            then [ CodeLine 1 "; }); }" ] 
                            else [ CodeLine 1 ";}" ]
            include    = C.pack $ "#include" ++ if mt then "<ass-mt.hpp>" else "<ass.hpp>"  


hasMain :: Source -> Bool
hasMain src =  ["int", "main", "("] `isInfixOf` (Cpp.toString <$> ts) 
                where ts = Cpp.tokenizer $ sourceFilter src


sourceFilter :: Source -> Source
sourceFilter = Cpp.filter Cpp.ContextFilter { Cpp.cppCode = True, Cpp.cppLiteral = True, Cpp.cppComment = False }   


getCompilerArgs :: [String] -> [String]
getCompilerArgs = takeWhile ( /= "--" )  


getTestArgs :: [String] -> [String]
getTestArgs = tailSafe . dropWhile ( /= "--" ) 


isPreprocessor :: SourceLine -> Bool
isPreprocessor = C.isPrefixOf "#" . C.dropWhile isSpace 


parseCodeLine :: ParserState -> CodeLine -> ParserState
parseCodeLine (t,m) (CodeLine n x)  
    | isPreprocessor x = (t ++ [CodeLine n x], m)
    | isSwitchLine x   = (m ++ [CodeLine n x], t)
    | otherwise        = (t, m ++ [CodeLine n x])
        where isSwitchLine = ("///" `C.isPrefixOf`)


toSourceCode :: Source -> SourceCode
toSourceCode src = zipWith CodeLine [1..] (C.lines src)


getCompilerOpt :: Compiler -> Bool -> [String]
getCompilerOpt comp@(Compiler _ bin _ opts) mt  
    | getCompilerFamily comp == Gcc =  
        case () of 
        _ | "4.8" `isSuffixOf` bin -> opt ++ opts ++ ["-std=c++11"] ++ pth ++ [ "-I/usr/local/include/4.8" ]  
          | "4.7" `isSuffixOf` bin -> opt ++ opts ++ ["-std=c++11"] ++ pth ++ [ "-I/usr/local/include/4.7" ] 
          | "4.6" `isSuffixOf` bin -> opt ++ opts ++ ["-std=c++0x"] ++ pth ++ [ "-I/usr/local/include/4.6" ] 
          | otherwise              -> opt ++ opts ++ ["-std=c++0x"] ++ pth 
            where opt = [ "-O0", "-D_GLIBCXX_DEBUG", "-Wall", "-Wextra", "-Wno-unused-parameter", "-Wno-unused-value" ]
                  pth | mt = ["-pthread"]
                      | otherwise = []

getCompilerOpt (Compiler _ _ _ opts) mt =   
        [ "-std=c++11", "-O0", "-D_GLIBCXX_DEBUG", "-Wall", "-include", pch, "-Wextra", "-Wno-unused-parameter", "-Wno-unneeded-internal-declaration"] ++ opts ++ pth
                where pch    | mt             = getCompilerPchPath opts ++ "ass-mt.hpp"
                             | otherwise      = getCompilerPchPath opts ++ "ass.hpp" 
                      pth    | mt = ["-pthread"]
                             | otherwise = []

getCompilerPchPath :: [String] -> String
getCompilerPchPath opts |  "-stdlib=libc++" `elem` opts = "/usr/local/include/clang-libc++/"
                    |  otherwise                        = "/usr/local/include/clang/"

compileWith :: Compiler -> FilePath -> FilePath -> Bool -> [String] -> IO ExitCode
compileWith cxx source binary mt user_opt = 
    -- print cmd >>
    system cmd
        where cmd = unwords . concat $ [[getCompilerExec cxx], getCompilerOpt cxx mt, user_opt, [source], ["-o"], [binary]] 


