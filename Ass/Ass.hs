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

import Control.Monad
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
                   "    -c  header      check header\n" ++
                   "    -l  file        launch interactive mode + load file\n" ++
                   "    -v, --version   show version\n" ++
                   "    -h, --help      print this help"


data REPLState =
    REPLState { replBanner       :: !Bool
              , replPreload      :: !Bool
              , replVerbose      :: !Bool
              , replFile         :: !FilePath
              , replCompiler     :: !CompilerType
              , replCompDir      :: !Bool
              , replArgs         :: ![String]
              , replPrepList     :: ![String]
              , replCode         :: ![String]
              } deriving (Show, Eq)


mkDefaultState file clist args code =
    REPLState { replBanner = True
              , replPreload  = False
              , replVerbose  = False
              , replFile     = file
              , replCompiler = compilerType $ head clist
              , replCompDir  = True
              , replArgs     = getRuntimeArgs args
              , replPrepList = []
              , replCode     = code
              }

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
            ("-c":xs:_)     -> getAvailCompilers clist >>= (\cl -> testCompileHeader (C.pack ("#include \"" ++ xs ++ "\"")) True [ head cl ] (getCompilerArgs (tail $ tail args)) >> putStrLn "Ok.")
            ("-i":_)        -> getAvailCompilers clist >>= mainLoop (tail args) ""
            ("-l":xs:_)     -> getAvailCompilers clist >>= mainLoop (tail $ tail args) xs
            _               -> liftM (head . compilerFilter cfamily) (getAvailCompilers clist) >>= mainFun args



type StateIO = StateT REPLState IO
type InputIO = InputT StateIO


getStringIdentifiers :: [String] -> [String]
getStringIdentifiers =
    nub . map Cpp.toString .
    filter Cpp.isIdentifier .
    Cpp.tokenizer . sourceCodeFilter .
    C.pack . unlines


commands, assIdentifiers :: [String]

commands = [ ":load", ":include", ":check", ":reload", ":rr", ":edit",
             ":list", ":clear", ":next", ":prev", ":args", ":run",
             ":info", ":compiler", ":preload", ":verbose", ":quit" ]

assIdentifiers = [ "hex", "oct", "bin", "T<", "type_name<", "type_of(",
                   "type_info_<", "SHOW(", "R(" , "P(" ]


replCompletion :: String -> String -> StateIO [Completion]
replCompletion l w = do
    s <- get
    files  <- liftIO $ filter (\f -> f /= "." && f /= "..") <$> getDirectoryContents "."
    case () of
       _ | "fni:" `isSuffixOf` l ->  return $ map simpleCompletion (filter (w `isPrefixOf`) $ getStringIdentifiers (replCode s))
       _ | "l:" `isSuffixOf`   l ->  return $ map simpleCompletion (filter (w `isPrefixOf`) files  )
       _ | "i:" `isSuffixOf`   l ->  return $ map simpleCompletion (filter (w `isPrefixOf`) files  )
       _ | "hc:" `isSuffixOf`  l ->  return $ map simpleCompletion (filter (w `isPrefixOf`) files  )
       _ | ":" `isPrefixOf`    w ->  return $ map simpleCompletion (filter (w `isPrefixOf`) commands )
       _                         ->  return $ map simpleCompletion (filter (w `isPrefixOf`) $ getStringIdentifiers (replCode s) ++ assIdentifiers)


mainLoop :: [String] -> FilePath -> [Compiler] -> IO ()
mainLoop args file clist = do

    putStrLn $ banner ++ " :? for help"
    putStr "Compilers found: " >> forM_ clist (\c -> putStr $ compilerName c ++ " " ) >> putChar '\n'

    home <- getHomeDirectory
    code <- if null file
                then return []
                else loadCode file

    let settings = setComplete (completeWordWithPrev Nothing " \t" replCompletion)
                        defaultSettings { historyFile = Just $ home </> ".ass_history" }

    unless (null file) $ putStrLn $ "Loading " ++ file

    evalStateT (runInputT settings loop) (mkDefaultState file clist args code)

    where
    loop :: InputIO ()
    loop = do
        s <- lift get
        handle (\e -> let msg = show (e :: SomeException) in
                          if msg /= "user interrupt" then outputStrLn msg >> loop
                                                     else loop) $
            if null $ compilerFilterType (replCompiler s) clist
                then lift (put $ s { replCompiler = (if replCompDir s then next
                                                                      else prec) (replCompiler s) } ) >> loop
                else do
                    when (replBanner s) $ outputStrLn $ "Using " ++ show (replCompiler s) ++ " compiler..."
                    in' <- getInputLine $ "Ass " ++ show (replCompiler s) ++ "> "
                    case words <$> in' of
                         Nothing -> outputStrLn "Leaving ASSi."
                         Just []                -> lift (put $ s{ replBanner = False }) >> loop
                         Just (":args":xs)      -> lift (put $ s{ replArgs = xs }) >> loop
                         Just (":preload":_)    -> outputStrLn ("Preloading headers (" ++ show (not $ replPreload s) ++ ")") >>
                                                   lift (put $ s{ replPreload = not $ replPreload s}) >> loop
                         Just (":verbose":_)    -> outputStrLn ("Verbose (" ++ show (not $ replVerbose s) ++ ")") >>
                                                   lift (put $ s{ replVerbose = not $ replVerbose s}) >> loop
                         Just (":clear":_)      -> outputStrLn "Buffer clean." >>
                                                   lift (put s { replBanner = True, replFile = "", replPrepList = [], replCode = [] }) >> loop
                         Just (":list":_)       -> mapM_ outputStrLn (replPrepList s) >> mapM_ outputStrLn (replCode s) >>
                                                   lift (put $ s {replBanner = False }) >> loop
                         Just (":edit":_)       -> mapM_ outputStrLn (replPrepList s) >> mapM_ outputStrLn (replCode s) >>
                                                   getCodeCmd >>= \xs -> lift (put s{ replBanner = False, replCode = replCode s ++ xs }) >> loop
                         Just [":include",h]    -> outputStrLn ("Including " ++ h ++ "...") >>
                                                   lift (put $ s{ replBanner = False, replCode = replCode s ++ ["#include <" ++ h ++ ">"] }) >> loop
                         Just [":load",f]       -> outputStrLn ("Loading " ++ f ++ "...") >>
                                                   loadCodeCmd f >>= \xs -> lift (put s{ replBanner = False, replFile = f, replCode = xs }) >> loop
                         Just [":check",f]      -> outputStrLn ("Checking " ++ f ++ "...") >> checkHeaderCmd f clist (getCompilerArgs args) >> loop
                         Just (":reload":_)     -> outputStrLn ("Reloading " ++ replFile s ++ "...") >>
                                                   reloadCodeCmd >>= \xs -> lift (put s{ replBanner = False, replCode = xs }) >> loop
                         Just (":quit":_)       -> void (outputStrLn "Leaving ASSi.")

                         Just (":next":_)       -> lift (put $ s{ replBanner = True, replCompDir = True,  replCompiler = next (replCompiler s)}) >> loop
                         Just (":prev":_)       -> lift (put $ s{ replBanner = True, replCompDir = False, replCompiler = prec (replCompiler s)}) >> loop

                         Just (":?":_)          -> lift printHelp >> lift (put $ s{ replBanner = True}) >> loop

                         Just (":rr" :xs)       -> do outputStrLn ("Reloading " ++ replFile s ++ "...")
                                                      reloadCodeCmd >>= \ys -> lift (put s{ replBanner = False, replCode = ys})
                                                      e <- runCmd "" clist (getCompilerArgs args) (if null xs then replArgs s else xs)
                                                      outputStrLn $ show e
                                                      lift (put $ s{ replBanner = False}) >> loop

                         Just (":run" :xs)      -> do e <- runCmd "" clist (getCompilerArgs args) (if null xs then replArgs s else xs)
                                                      outputStrLn $ show e
                                                      lift (put $ s{ replBanner = False}) >> loop

                         Just (":info":xs)      -> do e <- runCmd (if null xs then "" else "return type_info_<" ++ unwords xs ++ ">();") clist (getCompilerArgs args) (replArgs s)
                                                      outputStrLn $ show e
                                                      lift (put $ s{ replBanner = False}) >> loop

                         Just (":compiler":xs)  -> do liftIO $ print s
                                                      e <- runCmd "return compiler_info_();" clist (getCompilerArgs args) (replArgs s)
                                                      outputStrLn $ show e
                                                      lift (put $ s{ replBanner = False}) >> loop

                         Just input | ":" `isPrefixOf` unwords input -> outputStrLn("Unknown command '" ++ unwords input ++ "'") >>
                                                                        outputStrLn "use :? for help." >> lift (put $ s{ replBanner = False}) >> loop
                                    | isPreprocessor (C.pack $ unwords input) -> lift (put s{ replBanner = False, replPrepList = replPrepList s ++ [unwords input] }) >> loop
                                    | otherwise -> do e <- runCmd (unwords input) clist (getCompilerArgs args) (replArgs s)
                                                      outputStrLn $ show e
                                                      lift (put $ s{ replBanner = False}) >> loop

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
                        "  :prev                     switch to previous compiler\n" ++
                        "  :compiler                 dump compiler's information\n" ++
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



loadCode :: FilePath -> IO [String]
loadCode f = filter (not . ("#pragma" `isPrefixOf`) . dropWhite) <$> lines <$> readFile f


loadCodeCmd :: FilePath -> InputT StateIO [String]
loadCodeCmd f = liftIO $ loadCode f


reloadCodeCmd :: InputT StateIO [String]
reloadCodeCmd = lift get >>= \s ->
    if null (replFile s) then error "No file loaded!"
                         else loadCodeCmd $ replFile s


checkHeaderCmd :: FilePath -> [Compiler] -> [String] -> InputT StateIO [ExitCode]
checkHeaderCmd src clist cargs = do
    let code = ["#include \"" ++ src ++ "\""]
    lift get >>= \s ->
        liftIO $ testCompileHeader ((C.pack . unlines) code)
                    (replVerbose s)
                    (compilerFilterType (replCompiler s) clist)
                    cargs

runCmd :: FilePath -> [Compiler] -> [String] -> [String] -> InputT StateIO [ExitCode]
runCmd src clist cargs args = lift get >>= \s ->
    liftIO $ buildCompileAndRun (C.pack (unlines (replPrepList s) ++ unlines (replCode s)))
                  (C.pack src)
                  (replPreload s)
                  (replVerbose s)
                  (compilerFilterType (replCompiler s) clist)
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
    where cmdHeader = [ CodeLine 0 "auto ASS_XPASTE(__VOID_, __COUNTER__) = ass::eval([] {" ]
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

