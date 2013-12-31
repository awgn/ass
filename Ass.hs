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

import Data.List
import Data.Functor
import Safe (tailSafe)
import System.Environment(getArgs, getProgName)
import System.Process(system)
import System.IO

import System.Exit
import System.FilePath
import System.Directory
import System.Posix.User(getRealUserID)

import System.Console.Haskeline

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State.Strict

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


banner, snippet, assrc, ass_history :: String 
tmpDir, includeDir :: FilePath

banner      = "ASSi, version 1.3.7 :? for help"
snippet     = "snippet" 
tmpDir      =  "/tmp" 
includeDir  =  "/usr/local/include"
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

usage :: IO ()
usage = putStrLn $ "usage: ass [OPTION] [COMPILER OPT] -- [ARG]\n" ++
                   "    -i              launch interactive mode\n" ++
                   "    -h, --help      print this help"
                   
main :: IO ()
main = do args    <- getArgs
          home    <- getHomeDirectory
          cfamily <- getCompilerFamilyByName
          clist   <- getCompilerConf (home </> assrc)
          case args of
            ("-h":_)     -> usage
            ("--help":_) -> usage
            ("-?":_)     -> usage
            ("-i":_)     -> getCompilers clist >>= mainLoop (tail args)
            _            -> liftM (head . compFilter cfamily) (getCompilers clist) >>= mainFun args 


data CliState = CliState { stateBanner     :: Bool,
                           stateFile       :: FilePath,
                           stateCompType   :: CompilerType,
                           statePrepList   :: [String],
                           stateCode       :: [String]} deriving (Show, Eq)


type StateIO = StateT CliState IO
type InputIO = InputT StateIO


getStringIdentifiers :: [String] -> [String]
getStringIdentifiers xs =  
    nub $ map Cpp.toString $ 
    filter Cpp.isIdentifier $ 
    Cpp.tokenizer $ sourceCodeFilter $ 
    C.pack $ unlines xs

commands :: [String]
commands = [ ":load", ":include", ":reload", ":edit", ":show", ":clear", ":next", ":quit" ]

cliCompletion :: String -> String -> StateIO [Completion]
cliCompletion l s = do 
    state' <- get 
    files  <- lift $ liftM (filter (\f -> f /= "." && f /= "..")) $ getDirectoryContents "."
    case () of
       _ | "l:" `isSuffixOf` l ->  return $ map simpleCompletion (filter (s `isPrefixOf`) files  )    
       _ | "i:" `isSuffixOf` l ->  return $ map simpleCompletion (filter (s `isPrefixOf`) files  )    
       _ | ":" `isPrefixOf`  s ->  return $ map simpleCompletion (filter (s `isPrefixOf`) commands ) 
       _                       ->  return $ map simpleCompletion (filter (s `isPrefixOf`) (getStringIdentifiers $ stateCode state')) 
    

mainLoop :: [String] -> [Compiler] -> IO ()
mainLoop args clist = do
    putStrLn banner
    putStr "Compilers found: " >> mapM_ (\c -> putStr (getCompilerExec c ++ " ")) clist >> putChar '\n'
    home <- getHomeDirectory
    let startingState = CliState True "" (getCompilerType $ head clist) [] []
    let settings      = setComplete (completeWordWithPrev Nothing " \t" cliCompletion) defaultSettings { historyFile = Just $ home </> ".ass_history" }
    evalStateT (runInputT settings loop) startingState
    where
    loop :: InputIO ()
    loop = do
        state' <- lift get
        handle (\e -> let msg = show (e :: SomeException) in 
                          if msg /= "user interrupt" then outputStrLn msg >> loop  
                                                     else loop) $
            if null $ compFilterType (stateCompType state') clist 
            then  lift (put state'{ stateCompType = next (stateCompType state') }) >> loop  
            else do 
                when (stateBanner state') $ outputStrLn $ "Using " ++ show (stateCompType state') ++ " compiler..."
                minput <- getInputLine "Ass> "
                case words <$> minput of
                     Nothing -> outputStrLn "Leaving ASSi."
                     Just (":clear":_) -> outputStrLn "Buffer clean." >> 
                                        lift (put state'{ stateBanner = True, stateFile = "", statePrepList = [], stateCode = [] }) >> loop 
                     Just (":show":_) -> mapM_ outputStrLn (statePrepList state') >> 
                                         mapM_ outputStrLn (stateCode  state')    >> 
                                         lift (put state'{ stateBanner = False }) >> loop
                     Just (":edit":_) -> mapM_ outputStrLn (statePrepList state') >> 
                                         mapM_ outputStrLn (stateCode  state') >> 
                                         getCode >>= \xs -> lift (put state'{ stateBanner = False, stateCode = stateCode state' ++ xs }) >> loop
                     Just (":include":h:[]) -> outputStrLn ("Including " ++ h ++ "...") >> 
                                               lift (put state'{ stateBanner = False, stateCode = stateCode state' ++ ["#include <" ++ h ++ ">"] }) >> loop
                     Just (":load":f:[]) -> outputStrLn ("loading " ++ f ++ "...") >> 
                                            loadCode f >>= \xs -> lift (put state'{ stateBanner = False, stateFile = f, stateCode = xs }) >> loop
                     Just (":reload":_) -> outputStrLn ("Reloading " ++ stateFile state' ++ "...") >> 
                                           reloadCode >>= \xs -> lift (put state'{ stateBanner = False, stateCode = xs }) >> loop
                     Just (":quit":_) -> void (outputStrLn "Leaving ASSi.")
                     Just (":next":_) -> lift (put state'{ stateBanner = True, stateCompType = next (stateCompType state') }) >> loop
                     Just (":?":_)    -> lift printHelp >> lift (put state'{ stateBanner = True }) >> loop
                     Just []          -> lift (put state'{ stateBanner = False }) >> loop
                     Just input | isPreprocessor (C.pack $ unwords input) -> lift (put state'{ stateBanner = False, statePrepList = statePrepList state' ++ [unwords input] }) >> loop
                                | otherwise -> do 
                                  e <- lift $ lift $ buildCompileAndRun (C.pack(unlines (statePrepList state') ++ unlines (stateCode state'))) 
                                                                        (C.pack(unwords input)) True (compFilterType (stateCompType state') clist) (getCompilerArgs args) (getTestArgs args) 
                                  outputStrLn $ show e
                                  lift (put state'{ stateBanner = False }) >> loop


mainFun :: [String] -> Compiler -> IO ()
mainFun args cxx = do
    code <- C.hGetContents stdin
    liftM head
        (buildCompileAndRun code "" False [cxx] (getCompilerArgs args) (getTestArgs args)) >>= exitWith


printHelp :: StateIO ()
printHelp =  lift $ putStrLn $ "Commands available from the prompt:\n\n" ++
                        "<statement>                 evaluate/run C++ <statement>\n" ++
                        "  :include file             add include in the buffer\n" ++ 
                        "  :load file                load file in the buffer\n" ++ 
                        "  :reload                   reload the file\n" ++ 
                        "  :edit                     edit the buffer\n" ++ 
                        "  :show                     show the buffer\n" ++
                        "  :clear                    clear the buffer\n" ++ 
                        "  :next                     switch to next compiler\n" ++ 
                        "  :quit                     quit\n" ++
                        "  :?                        print this help\n\n" ++  
                        "C++ goodies:\n" ++
                        "  _(1,2,3)                  tuple/pair constructor\n" ++
                        "  P(arg1, arg2, ...)        variadic print\n" ++
                        "  S(instance)               stringify a value\n" ++
                        "  T<type>()                 demangle the name of a type\n" ++
                        "  R(1,2,5)                  range: initializer_list<int> {1,2,3,4,5}\n" ++
                        "  class O                   oracle class.\n"


getCode :: InputT StateIO [String]
getCode = do 
    line <- getInputLine "code> "
    case line of
         Nothing    -> return []
         Just []    -> getCode
         Just input -> (input :) <$> getCode


loadCode :: FilePath -> InputT StateIO [String]
loadCode f = lift . lift $ filter (not . ("#pragma" `isPrefixOf`) . dropWhite) <$> lines <$> readFile f 


reloadCode :: InputT StateIO [String]
reloadCode = lift get >>= \s -> 
    if null (stateFile s) then error "No file loaded!"
                          else loadCode $ stateFile s 


buildCompileAndRun :: Source -> Source -> Bool -> [Compiler] -> [String] -> [String] -> IO [ExitCode] 
-- buildCompileRun code main_code inter cxx cargs targs | trace ("buildCompileRun") False = undefined
buildCompileAndRun code main_code inter clist cargs targs = do 
    cwd' <- getCurrentDirectory
    uid  <- getRealUserID 
    let mt  = isMultiThread main_code cargs
    let bin = tmpDir </> snippet ++ "-" ++ show uid
    let src = bin <.> "cpp"
    writeSource src (makeSourceCode code main_code (getNamespaceInUse code) inter mt)
    forM clist $ \cxx -> do
        when (length clist > 1) $ putStr (show cxx ++ " -> ") >> hFlush stdout
        e <- compileWith cxx src (binary bin cxx) mt (["-I", cwd', "-I",  cwd' </> ".."] ++ cargs) 
        if e == ExitSuccess 
            then system (binary bin cxx ++ " " ++ unwords targs) 
            else return e
        where binary n c = n ++ "-" ++ show (getCompilerType c)


writeSource :: FilePath -> [SourceCode] -> IO ()
writeSource name src = writeFile name (unlines $ map show (concat src))


isMultiThread :: Source -> [String] -> Bool
isMultiThread src xs = "-pthread" `elem` xs  || useThreadOrAsync src 


useThreadOrAsync :: Source -> Bool
useThreadOrAsync src =  "thread" `elem` identifiers || "async" `elem` identifiers   
                            where tokens = filter Cpp.isIdentifier $ Cpp.tokenizer $ sourceCodeFilter src
                                  identifiers = Cpp.toString <$> tokens

getNamespaceInUse :: Source -> [String]
getNamespaceInUse src = filter (/= "{") $ map (Cpp.toString . (\i -> tokens !! (i + 1))) is  
                        where tokens = Cpp.tokenizer $ sourceCodeFilter src
                              is = findIndices (\token -> Cpp.isKeyword token && Cpp.toString token == "namespace") tokens 


makeSourceCode :: Source -> Source -> [String] -> Bool -> Bool -> [SourceCode]
-- makeSourceCode code main_code ns lambda mt | trace ("makeSourceCode code=" ++ (C.unpack code) ++ " main_code=" ++ (C.unpack main_code)) False = undefined
makeSourceCode code main_code ns lambda mt 
    | lambda       = [ zipSourceCode code, headers] ++ makeNamespaces ns  ++ [ mainHeader, zipSourceCode main_code, mainFooter ]
    | hasMain code = [ headers, zipSourceCode code] ++ makeNamespaces ns  ++ [ zipSourceCode main_code ]
    | otherwise    = [ headers, code' ] ++ makeNamespaces ns  ++ [ mainHeader, main_code', mainFooter ]
      where (code', main_code') = foldl parseCodeLine ([], []) (zipSourceCode code) 
            include    = C.pack $ "#include" ++ if mt then "<ass-mt.hpp>" else "<ass.hpp>"  
            headers    = [ CodeLine 1 include]
            mainHeader = if lambda 
                            then [ CodeLine 1 "int main(int argc, char *argv[]) { cout << boolalpha; ass::cmdline([&] {" ] 
                            else [ CodeLine 1 "int main(int argc, char *argv[]) { cout << boolalpha;" ]
            mainFooter = if lambda 
                            then [ CodeLine 1 "; }); }" ] 
                            else [ CodeLine 1 ";}" ]


makeNamespaces :: [String] -> [SourceCode]
makeNamespaces = map $ zipSourceCode . C.pack . (\n -> "using namespace " ++ n ++ ";")


hasMain :: Source -> Bool
hasMain src =  ["int", "main", "("] `isInfixOf` (Cpp.toString <$> ts) 
                where ts = Cpp.tokenizer $ sourceCodeFilter src


sourceCodeFilter :: Source -> Source
sourceCodeFilter = Cpp.filter Cpp.ContextFilter { Cpp.cppCode = True, Cpp.cppLiteral = True, Cpp.cppComment = False }   


getCompilerArgs :: [String] -> [String]
getCompilerArgs = takeWhile ( /= "--" )  


getTestArgs :: [String] -> [String]
getTestArgs = tailSafe . dropWhile ( /= "--" ) 


dropWhite :: String -> String
dropWhite = dropWhile (`elem` " \\\a\b\t\n\v\f\r") 


dropWhiteBS :: C.ByteString -> C.ByteString
dropWhiteBS =  C.dropWhile (`elem` " \\\a\b\t\n\v\f\r") 


isPreprocessor :: SourceLine -> Bool
isPreprocessor = C.isPrefixOf "#" . dropWhiteBS 


parseCodeLine :: ParserState -> CodeLine -> ParserState
parseCodeLine (t,m) (CodeLine n l)  
    | isMainLine  l = (t, m ++ [CodeLine n (C.pack $ delete '$' $ C.unpack l) ])
    | otherwise     = (t ++ [CodeLine n l], m)
        where isMainLine = ("$" `C.isPrefixOf`) . dropWhiteBS 


zipSourceCode :: Source -> SourceCode
zipSourceCode src = zipWith CodeLine [1..] (C.lines src)


getCompilerOpt :: Compiler -> Bool -> [String]
getCompilerOpt comp@(Compiler _ bin _ opts) mt  
    | getCompilerFamily comp == Gcc =  
        case () of 
        _ | "4.8" `isSuffixOf` bin -> opt ++ opts ++ ["-std=c++11"] ++ pth ++ [ "-I" ++ includeDir </> "4.8" ]  
          | "4.7" `isSuffixOf` bin -> opt ++ opts ++ ["-std=c++11"] ++ pth ++ [ "-I" ++ includeDir </> "4.7" ] 
          | "4.6" `isSuffixOf` bin -> opt ++ opts ++ ["-std=c++0x"] ++ pth ++ [ "-I" ++ includeDir </> "4.6" ] 
          | otherwise              -> opt ++ opts ++ ["-std=c++0x"] ++ pth 
            where opt = [ "-O0", "-D_GLIBCXX_DEBUG", "-Wall", "-Wextra", "-Wno-unused-parameter", "-Wno-unused-value" ]
                  pth | mt = ["-pthread"]
                      | otherwise = []


getCompilerOpt (Compiler _ _ _ opts) mt =   
        [ "-std=c++11", "-O0", "-D_GLIBCXX_DEBUG", "-Wall", "-include", pch, "-Wextra", "-Wno-unused-parameter", "-Wno-unneeded-internal-declaration"] ++ opts ++ pth
                where pch    | mt             = getCompilerPchPath opts </> "ass-mt.hpp"
                             | otherwise      = getCompilerPchPath opts </> "ass.hpp" 
                      pth    | mt = ["-pthread"]
                             | otherwise = []


getCompilerPchPath :: [String] -> String
getCompilerPchPath opts |  "-stdlib=libc++" `elem` opts = includeDir </> "clang-libc++"
                        |  otherwise                    = includeDir </> "clang"


compileWith :: Compiler -> FilePath -> FilePath -> Bool -> [String] -> IO ExitCode
compileWith cxx source binary mt user_opt = 
    system cmd
        where cmd = unwords . concat $ [[getCompilerExec cxx], getCompilerOpt cxx mt, user_opt, [source], ["-o"], [binary]] 


