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
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.List
import Data.Functor
import Safe (tailSafe)

import System.Environment(getArgs, getProgName)
import System.Process(system, readProcess)
import System.IO

import System.Exit
import System.FilePath
import System.Directory
import System.Posix.User(getEffectiveUserName)

import System.Console.Haskeline

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State.Strict

import qualified Data.ByteString.Char8 as C

import qualified Cpp.Source as Cpp
import qualified Cpp.Filter as Cpp
import qualified Cpp.Token  as Cpp


type Source          = Cpp.Source
data CodeLine        = CodeLine Int Source 

type SourceCode      = [CodeLine]
type TranslationUnit = SourceCode
type MainFunction    = SourceCode
type ParserState     = (TranslationUnit, MainFunction)
 
instance Show CodeLine where
    show (CodeLine n xs)
        | "\\" `C.isSuffixOf` xs = C.unpack xs  
        | otherwise              = C.unpack xs ++ "\n#line " ++ show n 

-- default compiler list (overridden by ~/.assrc)
--

compilerList :: [Compiler]
compilerList = [ 
                 Compiler Gcc48   "/usr/bin/g++-4.8" "g++-4.8" [],
                 Compiler Gcc47   "/usr/bin/g++-4.7" "g++-4.7" [],
                 Compiler Gcc46   "/usr/bin/g++-4.6" "g++-4.6" [],
                 Compiler Clang31 "/usr/bin/clang++" "clang++" [],
                 Compiler Clang32 "/usr/bin/clang++" "clang++" [],
                 Compiler Clang33 "/usr/bin/clang++" "clang++" [],
                 Compiler Clang34 "/usr/bin/clang++" "clang++" []
               ]


banner, snippet, assrc, ass_history :: String 
tmpDir, includeDir :: FilePath

banner      = "ASSi, version 2.8"
snippet     = "ass-snippet" 
tmpDir      =  "/tmp" 
includeDir  =  "/usr/local/include"
assrc       =  ".assrc"
ass_history = ".ass_history"


-- Compiler:

data CompilerType = Gcc46 | Gcc47 | Gcc48 | Gcc49 | Clang31 | Clang32 | Clang33 | Clang34 
                    deriving (Eq,Show,Read,Enum)


next :: CompilerType -> CompilerType
next Clang34 = Gcc46
next x = succ x


data CompilerFamily = Gcc | Clang 
    deriving (Eq,Show,Read,Enum)


data Compiler = Compiler CompilerType FilePath String [String] 
    deriving (Read, Eq)


instance Show Compiler where
    show (Compiler _ _ name opt) = name ++ " (" ++ show opt ++ ")"


getCompilerType :: Compiler -> CompilerType
getCompilerType (Compiler t _ _ _) = t


getCompilerVersion :: Compiler -> String
getCompilerVersion (Compiler Gcc46   _ _ _ ) = "4.6"
getCompilerVersion (Compiler Gcc47   _ _ _ ) = "4.7"
getCompilerVersion (Compiler Gcc48   _ _ _ ) = "4.8"
getCompilerVersion (Compiler Gcc49   _ _ _ ) = "4.9"
getCompilerVersion (Compiler Clang31 _ _ _ ) = "3.1"
getCompilerVersion (Compiler Clang32 _ _ _ ) = "3.2"
getCompilerVersion (Compiler Clang33 _ _ _ ) = "3.3"
getCompilerVersion (Compiler Clang34 _ _ _ ) = "3.4"

getCompilerFamily :: Compiler -> CompilerFamily
getCompilerFamily (Compiler Gcc46   _ _ _ ) = Gcc
getCompilerFamily (Compiler Gcc47   _ _ _ ) = Gcc
getCompilerFamily (Compiler Gcc48   _ _ _ ) = Gcc
getCompilerFamily (Compiler Gcc49   _ _ _ ) = Gcc
getCompilerFamily (Compiler Clang31 _ _ _ ) = Clang
getCompilerFamily (Compiler Clang32 _ _ _ ) = Clang
getCompilerFamily (Compiler Clang33 _ _ _ ) = Clang
getCompilerFamily (Compiler Clang34 _ _ _ ) = Clang


getCompilerExec :: Compiler -> FilePath
getCompilerExec (Compiler _ e _ _) = e


getCompilerName :: Compiler -> FilePath
getCompilerName (Compiler _ _ n _) = n


getCompilerExtraOpt :: Compiler -> [String]
getCompilerExtraOpt (Compiler _ _ _ xs) = xs


getAvailCompilers :: [Compiler] -> IO [Compiler]
getAvailCompilers = filterM (doesFileExist . getCompilerExec)   


validCompilers :: [Compiler] -> IO [Compiler]
validCompilers = filterM (\c -> (getCompilerVersion c `isPrefixOf`) <$> getRealCompilerVersion c) 


getRealCompilerVersion :: Compiler -> IO String
getRealCompilerVersion comp
    | Gcc <- getCompilerFamily comp = last . words . head . lines <$> readProcess (getCompilerExec comp) ["--version"] "" 
    | otherwise                     = last . words . head . lines <$> readProcess (getCompilerExec comp) ["--version"] "" 


getCompilerFamilyByName :: IO CompilerFamily
getCompilerFamilyByName = getProgName >>= \n ->  
    return $ if n `isSuffixOf` "clang" then Clang else Gcc 


compFilter :: CompilerFamily -> [Compiler] -> [Compiler]
compFilter t = filter $ (== t) . getCompilerFamily 


compFilterType :: CompilerType -> [Compiler] -> [Compiler]
compFilterType t = filter $ (== t) . getCompilerType 


getCompilerConf :: FilePath -> IO [Compiler]
getCompilerConf conf = 
    doesFileExist conf >>= \b -> 
        if b then read <$> readFile conf
             else return compilerList


usage :: IO ()
usage = putStrLn $ "usage: ass [OPTION] [COMPILER OPT] -- [ARG]\n" ++
                   "    -i              launch interactive mode\n" ++
                   "    -v, --version   show version\n" ++
                   "    -h, --help      print this help"


data CliState = CliState { _stateBanner     :: !Bool,
                           _statePreload    :: !Bool,
                           _stateVerbose    :: !Bool,
                           _stateFile       :: !FilePath,
                           _stateCompType   :: !CompilerType,
                           _stateArgs       :: ![String],
                           _statePrepList   :: ![String],
                           _stateCode       :: ![String]} deriving (Show, Eq)

makeLenses ''CliState


main :: IO ()
main = do args    <- getArgs
          home    <- getHomeDirectory
          cfamily <- getCompilerFamilyByName
          clist   <- getCompilerConf (home </> assrc)
          case args of
            ("-h":_)        -> usage
            ("--help":_)    -> usage
            ("-?":_)        -> usage
            ("-v":_)        -> putStrLn banner 
            ("--version":_) -> putStrLn banner 
            ("-i":_)        -> getAvailCompilers clist >>= validCompilers >>= mainLoop (tail args) 
            _               -> liftM (head . compFilter cfamily) (getAvailCompilers clist >>= validCompilers) >>= mainFun args 


type StateIO = StateT CliState IO
type InputIO = InputT StateIO


getStringIdentifiers :: [String] -> [String]
getStringIdentifiers =  
    nub . map Cpp.toString . 
    filter Cpp.isIdentifier . 
    Cpp.tokenizer . sourceCodeFilter . 
    C.pack . unlines 


commands, assIdentifiers :: [String]
commands = [ ":load", ":include", ":reload", ":edit", ":list", ":clear", ":next", ":args", ":run", ":xray", ":preload", ":verbose", ":quit" ]

assIdentifiers = [ "hex", "oct", "bin", "T<", "type_name<", "type_of(", "SHOW(", "R(" , "P(" , "Xray<" ]

cliCompletion :: String -> String -> StateIO [Completion]
cliCompletion l w = do 
    s <- get 
    files  <- liftIO $ filter (\f -> f /= "." && f /= "..") <$> getDirectoryContents "."
    case () of
       _ | "l:" `isSuffixOf` l ->  return $ map simpleCompletion (filter (w `isPrefixOf`) files  )    
       _ | "i:" `isSuffixOf` l ->  return $ map simpleCompletion (filter (w `isPrefixOf`) files  )    
       _ | "x:" `isSuffixOf` l ->  return $ map simpleCompletion (filter (w `isPrefixOf`) $ getStringIdentifiers (s^.stateCode))    
       _ | ":" `isPrefixOf`  w ->  return $ map simpleCompletion (filter (w `isPrefixOf`) commands ) 
       _                       ->  return $ map simpleCompletion (filter (w `isPrefixOf`) $ getStringIdentifiers (s^.stateCode) ++ assIdentifiers) 
    

mainLoop :: [String] -> [Compiler] -> IO ()
mainLoop args clist = do
    putStrLn $ banner ++ " :? for help"
    putStr "Compilers found: " >> mapM_ (\c -> putStr (getCompilerExec c ++ "(" ++ show (getCompilerType c) ++ ") ")) clist >> putChar '\n'
    home <- getHomeDirectory

    let startingState = CliState True False False "" (getCompilerType $ head clist) (getRuntimeArgs args) [] []
    let settings      = setComplete (completeWordWithPrev Nothing " \t" cliCompletion) defaultSettings { historyFile = Just $ home </> ".ass_history" }
     
    evalStateT (runInputT settings loop) startingState
    where
    loop :: InputIO ()
    loop = do
        s <- lift get
        handle (\e -> let msg = show (e :: SomeException) in 
                          if msg /= "user interrupt" then outputStrLn msg >> loop  
                                                     else loop) $
            if null $ compFilterType (s^.stateCompType) clist 
            then lift (put $ over stateCompType next s) >> loop  
            else do 
                when (s^.stateBanner) $ outputStrLn $ "Using " ++ show (s^.stateCompType) ++ " compiler..."
                in' <- getInputLine $ "Ass " ++ show (s^.stateCompType) ++ "> "
                case words <$> in' of
                     Nothing -> outputStrLn "Leaving ASSi."
                     Just []                -> lift (put $ stateBanner.~ False $ s) >> loop
                     Just (":args":xs)      -> lift (put $ stateArgs.~ xs $ s) >> loop 
                     Just (":preload":_)    -> outputStrLn ("Preloading headers (" ++ show (not $ s^.statePreload) ++ ")") >> 
                                               lift (put $ over statePreload not s) >> loop 
                     Just (":verbose":_)    -> outputStrLn ("Verbose (" ++ show (not $ s^.stateVerbose) ++ ")") >> 
                                               lift (put $ over stateVerbose not s) >> loop 
                     Just (":clear":_)      -> outputStrLn "Buffer clean." >> 
                                               lift (put s { _stateBanner = True, _stateFile = "", _statePrepList = [], _stateCode = [] }) >> loop 
                     Just (":list":_)       -> mapM_ outputStrLn (s^.statePrepList) >> mapM_ outputStrLn (s^.stateCode) >> 
                                               lift (put $ stateBanner.~ False $ s) >> loop
                     Just (":edit":_)       -> mapM_ outputStrLn (s^.statePrepList) >> mapM_ outputStrLn (s^.stateCode) >> 
                                               getCode >>= \xs -> lift (put s{ _stateBanner = False, _stateCode = s^.stateCode ++ xs }) >> loop
                     Just (":include":h:[]) -> outputStrLn ("Including " ++ h ++ "...") >> 
                                               lift (put $ s{ _stateBanner = False, _stateCode = s^.stateCode ++ ["#include <" ++ h ++ ">"] }) >> loop
                     Just (":load":f:[])    -> outputStrLn ("loading " ++ f ++ "...") >> 
                                               loadCode f >>= \xs -> lift (put s{ _stateBanner = False, _stateFile = f, _stateCode = xs }) >> loop
                     Just (":reload":_)     -> outputStrLn ("Reloading " ++ s^.stateFile ++ "...") >> 
                                               reloadCode >>= \xs -> lift (put s{ _stateBanner = False, _stateCode = xs }) >> loop
                     Just (":quit":_)       -> void (outputStrLn "Leaving ASSi.")
                     Just (":next":_)       -> lift (put $ stateBanner.~ True $ over stateCompType next s) >> loop
                     Just (":?":_)          -> lift printHelp >> lift (put $ stateBanner.~ True $ s) >> loop
                     Just (":run" :xs)      -> do e <- liftIO $ buildCompileAndRun (C.pack(unlines (s^.statePrepList) ++ unlines (s^.stateCode))) 
                                                                    "" 
                                                                    (s^.statePreload) 
                                                                    (s^.stateVerbose) 
                                                                    (compFilterType (s^.stateCompType) clist) 
                                                                    (getCompilerArgs args) 
                                                                    (if null xs then s^.stateArgs else xs)
                                                  outputStrLn $ show e  
                                                  lift (put $ stateBanner.~ False $ s) >> loop

                     Just (":xray":xs)      -> do e <- liftIO $ buildCompileAndRun (C.pack (unlines (s^.statePrepList) ++ unlines (s^.stateCode))) 
                                                                    (C.pack $ "return Xray<" ++ unwords xs ++ ">();") 
                                                                    (s^.statePreload) 
                                                                    (s^.stateVerbose) 
                                                                    (compFilterType (s^.stateCompType) clist) 
                                                                    (getCompilerArgs args) 
                                                                    (s^.stateArgs) 
                                                  outputStrLn $ show e  
                                                  lift (put $ stateBanner .~ False $ s) >> loop

                     Just input | ":" `isPrefixOf` (unwords input) -> outputStrLn("unknown command '" ++ unwords input ++ "'") >> 
                                                                      outputStrLn("use :? for help.") >> lift (put $ stateBanner .~ False $ s) >> loop
                                | isPreprocessor (C.pack $ unwords input) -> lift (put s{ _stateBanner = False, _statePrepList = s^.statePrepList ++ [unwords input] }) >> loop
                                | otherwise -> do e <- liftIO $ buildCompileAndRun (C.pack(unlines (s^.statePrepList) ++ unlines (s^.stateCode))) 
                                                                    (C.pack(unwords input)) 
                                                                    (s^.statePreload) 
                                                                    (s^.stateVerbose) 
                                                                    (compFilterType (s^.stateCompType) clist) 
                                                                    (getCompilerArgs args) 
                                                                    (s^.stateArgs) 
                                                  outputStrLn $ show e  
                                                  lift (put $ stateBanner .~ False $ s) >> loop

mainFun :: [String] -> Compiler -> IO ()
mainFun args cxx = do
    code <- C.hGetContents stdin
    head <$> buildCompileAndRun code "" True False [cxx] (getCompilerArgs args) (getRuntimeArgs args) >>= exitWith


printHelp :: StateIO ()
printHelp =  lift $ putStrLn $ "Commands available from the prompt:\n\n" ++
                        "<statement>                 evaluate/run C++ <statement>\n" ++
                        "  :include file             add include in the buffer\n" ++ 
                        "  :load file                load file in the buffer\n" ++ 
                        "  :reload                   reload the file\n" ++ 
                        "  :edit                     edit the buffer\n" ++ 
                        "  :list                     list the buffer\n" ++
                        "  :clear                    clear the buffer\n" ++ 
                        "  :next                     switch to next compiler\n" ++ 
                        "  :args ARG1 ARG2...        set runtime arguments\n" ++ 
                        "  :run [ARG1 ARG2...]       run main function\n" ++ 
                        "  :xray TYPE                show info about the given TYPE\n" ++
                        "  :preload                  toggle preload std headers\n" ++
                        "  :verbose                  show additional information\n" ++
                        "  :quit                     quit\n" ++
                        "  :?                        print this help\n\n" ++  
                        "C++ goodies:\n" ++
                        "  _s _h,_min,_s,_ms,_us...  string and chrono user-defined literals\n" ++
                        "  _(1,2,3)                  tuple/pair constructor\n" ++
                        "  P(arg1, arg2, ...)        variadic print\n" ++
                        "  T<type>()                 demangle the name of a type\n" ++
                        "  type_of(v)                deduce the type of a given expression\n" ++
                        "  R(1,2,5)                  range: initializer_list<int> {1,2,3,4,5}\n" ++
                        "  S(v),SHOW(v)              stringify a value\n" ++
                        "  hex(v), oct(v), bin(v)    show manipulators\n" ++
                        "  class O                   oracle class.\n"


getCode :: InputT StateIO [String]
getCode = do 
    line <- getInputLine "code> "
    case line of
         Nothing    -> return []
         Just []    -> getCode
         Just input -> (input :) <$> getCode


loadCode :: FilePath -> InputT StateIO [String]
loadCode f = liftIO $ filter (not . ("#pragma" `isPrefixOf`) . dropWhite) <$> lines <$> readFile f 


reloadCode :: InputT StateIO [String]
reloadCode = lift get >>= \s -> 
    if null (s^.stateFile) then error "No file loaded!"
                           else loadCode $ s^.stateFile


buildCompileAndRun :: Source -> Source -> Bool -> Bool -> [Compiler] -> [String] -> [String] -> IO [ExitCode] 
buildCompileAndRun code main_code preload verbose clist cargs targs = do 
    cwd' <- getCurrentDirectory
    name <- getEffectiveUserName 
    let boost = let ns = getQualifiedNamespace (code `C.append` main_code) in any (`elem` ns) ["b","boost"]
    let bin   = tmpDir </> snippet ++ "-" ++ name
    let src   = bin `addExtension` "cpp"
    writeSource src $ makeSourceCode code main_code (getDeclaredNamespace code) preload boost
    forM clist $ \cxx -> do
        when (length clist > 1) $ putStr (show cxx ++ " -> ") >> hFlush stdout
        e <- compileWith cxx src (binary bin cxx) verbose (["-I", cwd', "-I",  cwd' </> ".."] ++ cargs) 
        if e == ExitSuccess 
            then system (binary bin cxx ++ " " ++ unwords targs) 
            else return e
        where binary n c = n ++ "-" ++ show (getCompilerType c)


writeSource :: FilePath -> [SourceCode] -> IO ()
writeSource name src = writeFile name (unlines $ map show (concat src))


isMultiThread :: Source -> [String] -> Bool
isMultiThread src xs = "-pthread" `elem` xs  || useThreadOrAsync src 


useThreadOrAsync :: Source -> Bool
useThreadOrAsync src =  any (`elem` identifiers) ["thread", "async"] 
        where tokens = filter Cpp.isIdentifier $ Cpp.tokenizer $ sourceCodeFilter src
              identifiers = map Cpp.toString tokens


getQualifiedNamespace :: Source -> [String]
getQualifiedNamespace src = [ Cpp.toString t1 | [t1,t2] <- grps, 
                                Cpp.isOperOrPunct t2,
                                Cpp.toString t2 == "::" ]  
        where grps = spanGroup 2 $ Cpp.tokenizer $ sourceCodeFilter src
 

getDeclaredNamespace :: Source -> [String]
getDeclaredNamespace src =  [ t | [t1,t2] <- grps, let t = Cpp.toString t2, 
                                            Cpp.isKeyword t1,
                                            Cpp.toString t1 == "namespace", 
                                            t /= "{",
                                            not ("detail" `isInfixOf` t)] 
        where grps = spanGroup 2 $ Cpp.tokenizer $ sourceCodeFilter src


spanGroup :: Int -> [a] -> [[a]]
spanGroup _ [] = []
spanGroup 1 xs = map (: []) xs
spanGroup n xs | length xs >= n = take n xs : spanGroup n (tail xs)
               | otherwise      = []


makeSourceCode :: Source -> Source -> [String] -> Bool -> Bool -> [SourceCode]
makeSourceCode code cmd_code ns preload boost  
    = preloadHeaders preload headers code' ++ 
         makeNamespaces ns  ++ 
         concatMap makeCmdCode (groupBy (\_ _ -> False) cmd_code') ++ 
         (let cmd = concatMap makeCmdCode [zipSourceCode cmd_code] in if null cmd then [] else cmd ++ [exit]) ++ 
         [main']
      where (code', cmd_code') = foldl parseCodeLine ([], []) (zipSourceCode code) 
            main'   | hasMain code = [] 
                    | otherwise    = [ CodeLine 0 "int main() {}" ]
            exit                   = [ CodeLine 0 "auto __EXIT__ = ass::eval([]() { std::exit(0); }); "]                            
            headers                = makeInclude "<ass.hpp>" : [ makeInclude "<ass-boost.hpp>" | boost ]


preloadHeaders :: Bool -> SourceCode -> SourceCode -> [SourceCode]
preloadHeaders True  header code = [header, code]
preloadHeaders False header code = [code, header]



makeInclude :: String -> CodeLine
makeInclude s = CodeLine 1 (C.pack $ "#include " ++ s)


makeCmdCode :: SourceCode -> [SourceCode]
makeCmdCode [] = []
makeCmdCode code = [cmdHeader, code, cmdFooter] 
    where cmdHeader = [ CodeLine 0 "auto XPASTE(__VOID_, __COUNTER__) = ass::eval([] {" ] 
          cmdFooter = [ CodeLine 0 "});" ]


makeNamespaces :: [String] -> [SourceCode]
makeNamespaces = map $ zipSourceCode . C.pack . (\n -> "using namespace " ++ n ++ ";")


hasMain :: Source -> Bool
hasMain src =  ["int", "main", "("] `isInfixOf` map Cpp.toString ts 
                where ts = Cpp.tokenizer $ sourceCodeFilter src


sourceCodeFilter :: Source -> Source
sourceCodeFilter = Cpp.filter Cpp.ContextFilter { Cpp.cppCode = True, Cpp.cppLiteral = True, Cpp.cppComment = False }   


getCompilerArgs :: [String] -> [String]
getCompilerArgs = takeWhile ( /= "--" )  


getRuntimeArgs :: [String] -> [String]
getRuntimeArgs = tailSafe . dropWhile ( /= "--" ) 


dropWhite :: String -> String
dropWhite = dropWhile (`elem` " \\\a\b\t\n\v\f\r") 


dropWhiteBS :: C.ByteString -> C.ByteString
dropWhiteBS =  C.dropWhile (`elem` " \\\a\b\t\n\v\f\r") 


isPreprocessor :: Source -> Bool
isPreprocessor = C.isPrefixOf "#" . dropWhiteBS 


parseCodeLine :: ParserState -> CodeLine -> ParserState
parseCodeLine (t,m) (CodeLine n l)  
    | isCmdLine  l = (t, m ++ [CodeLine n (C.pack $ delete '$' $ C.unpack l) ])
    | otherwise    = (t ++ [CodeLine n l], m)
        where isCmdLine = ("$" `C.isPrefixOf`) . dropWhiteBS 


zipSourceCode :: Source -> SourceCode
zipSourceCode src = zipWith CodeLine [2..] (C.lines src)


getCompilerOpt :: Compiler -> [String]
getCompilerOpt (Compiler ver _ _ opts) = 
        case ver of 
         Gcc49   -> gcc_opt ++ opts ++ ["-std=c++11", "-I" ++ includeDir </> "4.9" ] 
         Gcc48   -> gcc_opt ++ opts ++ ["-std=c++11", "-I" ++ includeDir </> "4.8" ]  
         Gcc47   -> gcc_opt ++ opts ++ ["-std=c++11", "-I" ++ includeDir </> "4.7" ] 
         Gcc46   -> gcc_opt ++ opts ++ ["-std=c++0x", "-I" ++ includeDir </> "4.6" ] 
         Clang31 -> clg_opt ++ opts ++ ["-std=c++11"] ++ pch
         Clang32 -> clg_opt ++ opts ++ ["-std=c++11"] ++ pch
         Clang33 -> clg_opt ++ opts ++ ["-std=c++11"] ++ pch
         Clang34 -> clg_opt ++ opts ++ ["-std=c++11"] ++ pch
    where gcc_opt = [ "-O0", "-D_GLIBCXX_DEBUG", "-pthread", "-Wall", "-Wextra", "-Wno-unused-parameter", "-Wno-unused-value", "-Winvalid-pch" ]
          clg_opt = [ "-O0", "-D_GLIBCXX_DEBUG", "-pthread", "-Wall", "-Wextra", "-Wno-unused-parameter", "-Wno-unused-value", "-Wno-unneeded-internal-declaration"] 
          pch     = ["-include ", getCompilerPchPath opts </> "ass.hpp" ]   

                                                                  
getCompilerPchPath :: [String] -> String
getCompilerPchPath opts |  "-stdlib=libc++" `elem` opts = includeDir </> "clang-libc++"
                        |  otherwise                    = includeDir </> "clang"


compileWith :: Compiler -> FilePath -> FilePath -> Bool -> [String] -> IO ExitCode
compileWith cxx source binary verbose user_opt = 
    when verbose (putStrLn cmd) >> system cmd
        where cmd = unwords . concat $ [[getCompilerExec cxx], getCompilerOpt cxx, user_opt, [source], ["-o"], [binary]] 


