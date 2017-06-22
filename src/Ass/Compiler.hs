--
-- Copyright (c) 2011-16 Bonelli Nicola <bonelli@antifork.org>
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
-- ass: C++11/14 code assistant

module Ass.Compiler where

import Ass.Config

import System.FilePath
import System.Directory
import System.Process
import System.Environment
import System.Exit

import Control.Monad

import Data.List
import Data.Monoid
import Data.Char (isSpace)

defaultCompilerList :: [Compiler]
defaultCompilerList =
    [
        Compiler Gcc7    Gcc    "/usr/bin/g++-7"   "g++-7"   ["-std=c++1z", "-fdiagnostics-color=always"]
    ,   Compiler Gcc6    Gcc    "/usr/bin/g++-6"   "g++-6"   ["-std=c++1z", "-fdiagnostics-color=always"]
    ,   Compiler Gcc5    Gcc    "/usr/bin/g++-5"   "g++-5"   ["-std=c++1y", "-fdiagnostics-color=always"]
    ,   Compiler Gcc49   Gcc    "/usr/bin/g++-4.9" "g++-4.9" ["-std=c++1y", "-fdiagnostics-color=always"]
    ,   Compiler Gcc48   Gcc    "/usr/bin/g++-4.8" "g++-4.8" ["-std=c++11"]
    ,   Compiler Gcc47   Gcc    "/usr/bin/g++-4.7" "g++-4.7" ["-std=c++11"]
    ,   Compiler Gcc46   Gcc    "/usr/bin/g++-4.6" "g++-4.6" ["-std=c++0x"]
    ,   Compiler Clang39 Clang  "/usr/bin/clang++" "clang++-39" ["-std=c++14", "-stdlib=libc++"]
    ,   Compiler Clang38 Clang  "/usr/bin/clang++" "clang++-38" ["-std=c++14", "-stdlib=libc++"]
    ,   Compiler Clang37 Clang  "/usr/bin/clang++" "clang++-37" ["-std=c++14", "-stdlib=libc++"]
    ,   Compiler Clang36 Clang  "/usr/bin/clang++" "clang++-36" ["-std=c++14", "-stdlib=libc++"]
    ,   Compiler Clang35 Clang  "/usr/bin/clang++" "clang++-35" ["-std=c++1y", "-stdlib=libc++"]
    ,   Compiler Clang34 Clang  "/usr/bin/clang++" "clang++-34" ["-std=c++1y", "-stdlib=libc++"]
    ,   Compiler Clang33 Clang  "/usr/bin/clang++" "clang++-33" ["-std=c++11", "-stdlib=libc++"]
    ,   Compiler Clang32 Clang  "/usr/bin/clang++" "clang++-32" ["-std=c++11", "-stdlib=libc++"]
    ,   Compiler Clang31 Clang  "/usr/bin/clang++" "clang++-31" ["-std=c++11", "-stdlib=libc++"]
    ]


-- Compiler:

data CompilerType = Gcc46 | Gcc47 | Gcc48 | Gcc49 | Gcc5 | Gcc6 | Gcc7 | Clang31 | Clang32 | Clang33 | Clang34 | Clang35 | Clang36 | Clang37 | Clang38 | Clang39
                    deriving (Eq,Show,Read,Enum,Bounded)


data CompilerFamily = Gcc | Clang
    deriving (Eq,Show,Read,Enum,Bounded)


next, prec :: CompilerType -> CompilerType

next comp = if comp == maxBound then minBound else succ comp
prec comp = if comp == minBound then maxBound else pred comp


data Compiler = Compiler
    {   compilerType    :: CompilerType
    ,   compilerFamily  :: CompilerFamily
    ,   compilerExec    :: FilePath
    ,   compilerName    :: String
    ,   compilerOpts    :: [String]
    }
    deriving (Read, Show, Eq)


getCompilerVersion :: CompilerType -> String
getCompilerVersion Gcc46   = "4.6"
getCompilerVersion Gcc47   = "4.7"
getCompilerVersion Gcc48   = "4.8"
getCompilerVersion Gcc49   = "4.9"
getCompilerVersion Gcc5    = "5"
getCompilerVersion Gcc6    = "6"
getCompilerVersion Gcc7    = "7"
getCompilerVersion Clang31 = "3.1"
getCompilerVersion Clang32 = "3.2"
getCompilerVersion Clang33 = "3.3"
getCompilerVersion Clang34 = "3.4"
getCompilerVersion Clang35 = "3.5"
getCompilerVersion Clang36 = "3.6"
getCompilerVersion Clang37 = "3.7"
getCompilerVersion Clang38 = "3.8"
getCompilerVersion Clang39 = "3.9"


clean :: String -> String
clean =  unlines . filter notComment . lines
    where notComment = (not . ("#" `isPrefixOf`)) . dropWhile isSpace


getCompilerConf :: FilePath -> IO [Compiler]
getCompilerConf conf =
    doesFileExist conf >>= \b ->
        if b then (read . clean) <$> readFile conf
             else return defaultCompilerList


getAvailCompilers :: [Compiler] -> IO [Compiler]
getAvailCompilers xs = do
    ys <- filterM (doesFileExist . compilerExec) xs
    zs <- filterM isValidCompiler ys
    ps <- mapM (canonicalizePath . compilerExec) zs
    let out =  map fst (nubBy (\(_,p1) (_,p2) -> p1 == p2) $ zip zs ps)
    return out


isValidCompiler :: Compiler -> IO Bool
isValidCompiler c =  ((getCompilerVersion . compilerType) c `isPrefixOf`) <$> askCompilerVersion c


askCompilerVersion :: Compiler -> IO String
askCompilerVersion comp
    | Gcc <- compilerFamily comp = readProcess (compilerExec comp) ["-dumpversion"] ""
    | otherwise {- clang -}      = do
        ws  <- words . head . lines <$> readProcess (compilerExec comp) ["--version"] ""
        case ws of
          "version" : x : _         -> return x
          _ : "version" : x : _     -> return x
          _ : _ : "version" : x : _ -> return x
          _                         -> return "?"


getCompilerFamilyByName :: IO CompilerFamily
getCompilerFamilyByName = getProgName >>= \n ->
    return $ if n `isSuffixOf` "clang" then Clang else Gcc


compilerFilter :: CompilerFamily -> [Compiler] -> [Compiler]
compilerFilter t = filter $ (== t) . compilerFamily


compilerFilterType :: CompilerType -> [Compiler] -> [Compiler]
compilerFilterType t = filter $ (== t) . compilerType


getCompilerOpt :: Compiler -> [String]
getCompilerOpt (Compiler ver _ _ _ opts) =
    case () of
      _ | ver `elem` [Gcc46, Gcc47, Gcc48, Gcc49, Gcc5, Gcc6, Gcc7 ] -> gcc_opt <> opts
        | otherwise                                                  -> clg_opt <> opts
    where gcc_opt = [ "-O0", "-D_GLIBCXX_DEBUG", "-pthread", "-Wall", "-Wextra", "-Wno-unused-parameter", "-Wno-unused-value" ]
          clg_opt = [ "-O0", "-D_GLIBCXX_DEBUG", "-pthread", "-Wall", "-Wextra", "-Wno-unused-parameter", "-Wno-unused-value", "-Wno-unneeded-internal-declaration"]


getCompilerOptPCH :: Compiler -> [String]
getCompilerOptPCH comp@(Compiler ver _ _ _ _) =
    case () of
    _ | ver ==  Gcc46   -> getCompilerOpt comp ++ [ "-Winvalid-pch", "-I" ++ includeAssDir </> "4.6" ]  ++ ["-I" ++ includeAssDir ]
      | ver ==  Gcc47   -> getCompilerOpt comp ++ [ "-Winvalid-pch", "-I" ++ includeAssDir </> "4.7" ]  ++ ["-I" ++ includeAssDir ]
      | ver ==  Gcc48   -> getCompilerOpt comp ++ [ "-Winvalid-pch", "-I" ++ includeAssDir </> "4.8" ]  ++ ["-I" ++ includeAssDir ]
      | ver ==  Gcc49   -> getCompilerOpt comp ++ [ "-Winvalid-pch", "-I" ++ includeAssDir </> "4.9" ]  ++ ["-I" ++ includeAssDir ]
      | ver ==  Gcc5    -> getCompilerOpt comp ++ [ "-Winvalid-pch", "-I" ++ includeAssDir </> "5" ]    ++ ["-I" ++ includeAssDir ]
      | ver ==  Gcc6    -> getCompilerOpt comp ++ [ "-Winvalid-pch", "-I" ++ includeAssDir </> "6" ]    ++ ["-I" ++ includeAssDir ]
      | ver ==  Gcc7    -> getCompilerOpt comp ++ [ "-Winvalid-pch", "-I" ++ includeAssDir </> "7" ]    ++ ["-I" ++ includeAssDir ]
      | otherwise       -> getCompilerOpt comp ++ ["-include ", getCompilerPchPath comp </> "ass.hpp" ] ++ ["-I" ++ includeAssDir ]


getCompilerPchPath ::  Compiler -> FilePath
getCompilerPchPath (Compiler ver _ _ _ opts) =
    case ver of
         Gcc46   -> includeAssDir </> "4.6"
         Gcc47   -> includeAssDir </> "4.7"
         Gcc48   -> includeAssDir </> "4.8"
         Gcc49   -> includeAssDir </> "4.9"
         Gcc5    -> includeAssDir </> "5"
         Gcc6    -> includeAssDir </> "6"
         Gcc7    -> includeAssDir </> "7"
         Clang31 -> includeAssDir </> "clang31" </> clangDir
         Clang32 -> includeAssDir </> "clang32" </> clangDir
         Clang33 -> includeAssDir </> "clang33" </> clangDir
         Clang34 -> includeAssDir </> "clang34" </> clangDir
         Clang35 -> includeAssDir </> "clang35" </> clangDir
         Clang36 -> includeAssDir </> "clang36" </> clangDir
         Clang37 -> includeAssDir </> "clang37" </> clangDir
         Clang38 -> includeAssDir </> "clang38" </> clangDir
         Clang39 -> includeAssDir </> "clang39" </> clangDir
    where
        clangDir |  "-std=c++1y" `elem` opts && "-stdlib=libc++" `elem` opts = "libc++1y"
                 |  "-stdlib=libc++" `elem` opts                             = "libc++"
                 |  otherwise                                                = "glibcxx"


runCompiler :: Compiler -> [FilePath] -> FilePath -> Bool -> [String] -> IO ExitCode
runCompiler cxx sources binary verbose user_opt =
    when verbose (putStrLn cmd) >> system cmd
        where cmd = unwords . concat $ [[compilerExec cxx], getCompilerOptPCH cxx, user_opt, sources, ["-o"], [binary]]


