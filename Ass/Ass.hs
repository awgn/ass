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
-- ass: C++11 code assistant for vim

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.List
import Data.Functor
import Safe (tailSafe)

import System.Environment(getArgs)
import System.Process(system)
import System.IO

import System.Exit
import System.Posix.User(getEffectiveUserName)
import System.FilePath
import System.Directory

import System.Console.Haskeline

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State.Strict

import qualified Data.ByteString.Char8 as C

import qualified Ass.Cpp.Filter as Cpp
import qualified Ass.Cpp.Token  as Cpp

import Ass.Config
import Ass.Compiler
import Ass.Types


-- import Debug.Trace


usage :: IO ()
usage = putStrLn $ "usage: ass [OPTION] [COMPILER OPT] -- [ARG]\n" ++
                   "    -i              launch interactive mode\n" ++
                   "    -l  file        launch interactive mode + load file\n" ++
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
            ("-i":_)        -> getAvailCompilers clist >>= getValidCompilers >>= mainLoop (tail args) ""
            ("-l":xs:_)     -> getAvailCompilers clist >>= getValidCompilers >>= mainLoop (tail $ tail args) xs
            _               -> liftM (head . compilerFilter cfamily) (getAvailCompilers clist >>= getValidCompilers) >>= mainFun args


type StateIO = StateT CliState IO
type InputIO = InputT StateIO


getStringIdentifiers :: [String] -> [String]
getStringIdentifiers =
    nub . map Cpp.toString .
    filter Cpp.isIdentifier .
    Cpp.tokenizer . sourceCodeFilter .
    C.pack . unlines


commands, assIdentifiers :: [String]

commands = [ ":load", ":include", ":check", ":reload", ":rr", ":edit",
             ":list", ":clear", ":next", ":args", ":run",
             ":info", ":preload", ":verbose", ":quit" ]

assIdentifiers = [ "hex", "oct", "bin", "T<", "type_name<", "type_of(",
                   "type_info_<", "SHOW(", "R(" , "P(" ]


cliCompletion :: String -> String -> StateIO [Completion]
cliCompletion l w = do
    s <- get
    files  <- liftIO $ filter (\f -> f /= "." && f /= "..") <$> getDirectoryContents "."
    case () of
       _ | "fni:" `isSuffixOf` l ->  return $ map simpleCompletion (filter (w `isPrefixOf`) $ getStringIdentifiers (s^.stateCode))
       _ | "l:" `isSuffixOf`   l ->  return $ map simpleCompletion (filter (w `isPrefixOf`) files  )
       _ | "i:" `isSuffixOf`   l ->  return $ map simpleCompletion (filter (w `isPrefixOf`) files  )
       _ | "hc:" `isSuffixOf`  l ->  return $ map simpleCompletion (filter (w `isPrefixOf`) files  )
       _ | ":" `isPrefixOf`    w ->  return $ map simpleCompletion (filter (w `isPrefixOf`) commands )
       _                         ->  return $ map simpleCompletion (filter (w `isPrefixOf`) $ getStringIdentifiers (s^.stateCode) ++ assIdentifiers)


mainLoop :: [String] -> FilePath -> [Compiler] -> IO ()
mainLoop args file clist = do

    putStrLn $ banner ++ " :? for help"
    putStr "Compilers found: " >> forM_ clist (\c -> putStr $ compilerName c ++ " " ) >> putChar '\n'

    home <- getHomeDirectory

    code <- if null file
                then return []
                else loadCodeCmd' file

    let startingState = CliState
                        {
                            _stateBanner   = True,
                            _statePreload  = False,
                            _stateVerbose  = False,
                            _stateFile     = file,
                            _stateCompType = compilerType $ head clist,
                            _stateArgs     = getRuntimeArgs args,
                            _statePrepList = [],
                            _stateCode     = code
                        }

    let settings = setComplete (completeWordWithPrev Nothing " \t" cliCompletion) defaultSettings { historyFile = Just $ home </> ".ass_history" }

    unless (null file) $ putStrLn $ "Loading " ++ file

    evalStateT (runInputT settings loop) startingState

    where
    loop :: InputIO ()
    loop = do
        s <- lift get
        handle (\e -> let msg = show (e :: SomeException) in
                          if msg /= "user interrupt" then outputStrLn msg >> loop
                                                     else loop) $
            if null $ compilerFilterType (s^.stateCompType) clist
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
                                               getCodeCmd >>= \xs -> lift (put s{ _stateBanner = False, _stateCode = s^.stateCode ++ xs }) >> loop
                     Just [":include",h]    -> outputStrLn ("Including " ++ h ++ "...") >>
                                               lift (put $ s{ _stateBanner = False, _stateCode = s^.stateCode ++ ["#include <" ++ h ++ ">"] }) >> loop
                     Just [":load",f]       -> outputStrLn ("Loading " ++ f ++ "...") >>
                                               loadCodeCmd f >>= \xs -> lift (put s{ _stateBanner = False, _stateFile = f, _stateCode = xs }) >> loop
                     Just [":check",f]      -> outputStrLn ("Checking " ++ f ++ "...") >> checkHeaderCmd f clist (getCompilerArgs args) >> loop
                     Just (":reload":_)     -> outputStrLn ("Reloading " ++ s^.stateFile ++ "...") >>
                                               reloadCodeCmd >>= \xs -> lift (put s{ _stateBanner = False, _stateCode = xs }) >> loop
                     Just (":quit":_)       -> void (outputStrLn "Leaving ASSi.")
                     Just (":next":_)       -> lift (put $ stateBanner.~ True $ over stateCompType next s) >> loop
                     Just (":?":_)          -> lift printHelp >> lift (put $ stateBanner.~ True $ s) >> loop

                     Just (":rr" :xs)       -> do outputStrLn ("Reloading " ++ s^.stateFile ++ "...")
                                                  reloadCodeCmd >>= \ys -> lift (put s{ _stateBanner = False, _stateCode = ys})
                                                  e <- runCmd "" clist (getCompilerArgs args) (if null xs then s^.stateArgs else xs)
                                                  outputStrLn $ show e
                                                  lift (put $ stateBanner.~ False $ s) >> loop

                     Just (":run" :xs)      -> do e <- runCmd "" clist (getCompilerArgs args) (if null xs then s^.stateArgs else xs)
                                                  outputStrLn $ show e
                                                  lift (put $ stateBanner.~ False $ s) >> loop

                     Just (":info":xs)      -> do e <- runCmd (if null xs then "" else "return type_info_<" ++ unwords xs ++ ">();") clist (getCompilerArgs args) (s^.stateArgs)
                                                  outputStrLn $ show e
                                                  lift (put $ stateBanner .~ False $ s) >> loop

                     Just input | ":" `isPrefixOf` unwords input -> outputStrLn("Unknown command '" ++ unwords input ++ "'") >>
                                                                    outputStrLn "use :? for help." >> lift (put $ stateBanner .~ False $ s) >> loop
                                | isPreprocessor (C.pack $ unwords input) -> lift (put s{ _stateBanner = False, _statePrepList = s^.statePrepList ++ [unwords input] }) >> loop
                                | otherwise -> do e <- runCmd (unwords input) clist (getCompilerArgs args) (s^.stateArgs)
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
                        "  :check header             perform compile test on cpp header\n" ++
                        "  :reload                   reload the file\n" ++
                        "  :edit                     edit the buffer\n" ++
                        "  :list                     list the buffer\n" ++
                        "  :clear                    clear the buffer\n" ++
                        "  :next                     switch to next compiler\n" ++
                        "  :args ARG1 ARG2...        set runtime arguments\n" ++
                        "  :run [ARG1 ARG2...]       run the main function\n" ++
                        "  :rr                       reload and run the main function\n" ++
                        "  :info TYPE                show info about the given TYPE\n" ++
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


getCodeCmd :: InputT StateIO [String]
getCodeCmd = do
    line <- getInputLine "code> "
    case line of
         Nothing    -> return []
         Just []    -> getCodeCmd
         Just input -> (input :) <$> getCodeCmd


loadCodeCmd' :: FilePath -> IO [String]
loadCodeCmd' f = filter (not . ("#pragma" `isPrefixOf`) . dropWhite) <$> lines <$> readFile f


loadCodeCmd :: FilePath -> InputT StateIO [String]
loadCodeCmd f = liftIO $ loadCodeCmd' f


reloadCodeCmd :: InputT StateIO [String]
reloadCodeCmd = lift get >>= \s ->
    if null (s^.stateFile) then error "No file loaded!"
                           else loadCodeCmd $ s^.stateFile


checkHeaderCmd :: FilePath -> [Compiler] -> [String] -> InputT StateIO [ExitCode]
checkHeaderCmd src clist cargs = do
    code <- loadCodeCmd src
    lift get >>= \s ->
        liftIO $ testCompileHeader ((C.pack . unlines) code)
                    (s^.stateVerbose)
                    (compilerFilterType (s^.stateCompType) clist)
                    cargs

runCmd :: FilePath -> [Compiler] -> [String] -> [String] -> InputT StateIO [ExitCode]
runCmd src clist cargs args = lift get >>= \s ->
    liftIO $ buildCompileAndRun (C.pack (unlines (s^.statePrepList) ++ unlines (s^.stateCode)))
                  (C.pack src)
                  (s^.statePreload)
                  (s^.stateVerbose)
                  (compilerFilterType (s^.stateCompType) clist)
                  cargs
                  args


testCompileHeader :: Source -> Bool -> [Compiler] -> [String] -> IO [ExitCode]
testCompileHeader code verbose clist cargs = do
    cwd' <- getCurrentDirectory
    name <- getEffectiveUserName
    let bin = tmpDir </> snippet ++ "-" ++ name
    let src1 = bin `addExtension` "cpp"
    let src2 = (bin ++ "-2") `addExtension` "cpp"
    writeSource src1 $ makeSourceCode code "" (getDeclaredNamespace code) False False
    C.writeFile src2 code
    forM clist $ \ cxx ->
        runCompiler cxx [src1, src2] (binary bin cxx) verbose (["-I", cwd', "-I",  cwd' </> ".."] ++ cargs)
        where binary n c = n ++ "-" ++ show (compilerType c)


buildCompileAndRun :: Source -> Source -> Bool -> Bool -> [Compiler] -> [String] -> [String] -> IO [ExitCode]
buildCompileAndRun code main_code preload verbose clist cargs targs = do
    cwd' <- getCurrentDirectory
    name <- getEffectiveUserName
    let boost = let ns = getQualifiedNamespace (code `C.append` main_code) in any (`elem` ns) ["b","boost"]
    let bin   = tmpDir </> snippet ++ "-" ++ name
    let src   = bin `addExtension` "cpp"
    writeSource src $ makeSourceCode code main_code (getDeclaredNamespace code) preload boost
    forM clist $ \cxx -> do
        when (length clist > 1) $ putStr (compilerName cxx ++ " -> ") >> hFlush stdout
        e <- runCompiler cxx [src] (binary bin cxx) verbose (["-I", cwd', "-I",  cwd' </> ".."] ++ cargs)
        if e == ExitSuccess
            then system (binary bin cxx ++ " " ++ unwords targs)
            else return e
        where binary n c = n ++ "-" ++ show (compilerType c)


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
                    | otherwise    = [ CodeLine 0 "int main() { return 0; }" ]
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

