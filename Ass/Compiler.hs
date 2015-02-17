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

module Ass.Compiler where

import Ass.Config

import System.FilePath
import System.Directory
import System.Process
import System.Environment
import System.Exit
import System.Posix.Files

import Control.Monad
import Control.Applicative

import Data.List


defaultCompilerList :: [Compiler]
defaultCompilerList =
    [
        Compiler Gcc49   "/usr/bin/g++-4.9" "g++-4.9" [],
        Compiler Gcc48   "/usr/bin/g++-4.8" "g++-4.8" [],
        Compiler Gcc47   "/usr/bin/g++-4.7" "g++-4.7" [],
        Compiler Gcc46   "/usr/bin/g++-4.6" "g++-4.6" [],
        Compiler Clang36 "/usr/bin/clang++" "clang++" [],
        Compiler Clang35 "/usr/bin/clang++" "clang++" [],
        Compiler Clang34 "/usr/bin/clang++" "clang++" [],
        Compiler Clang33 "/usr/bin/clang++" "clang++" [],
        Compiler Clang32 "/usr/bin/clang++" "clang++" [],
        Compiler Clang31 "/usr/bin/clang++" "clang++" []
    ]


-- Compiler:

data CompilerType = Gcc46 | Gcc47 | Gcc48 | Gcc49 | Clang31 | Clang32 | Clang33 | Clang34 | Clang35 | Clang36
                    deriving (Eq,Show,Read,Enum)


next :: CompilerType -> CompilerType
next Clang36 = Gcc46
next x       = succ x


data CompilerFamily = Gcc | Clang
    deriving (Eq,Show,Read,Enum)


data Compiler = Compiler
                {
                    compilerType :: CompilerType,
                    compilerExec :: FilePath,
                    compilerName :: String,
                    compilerOpts :: [String]
                }
    deriving (Read, Show, Eq)


getCompilerVersion :: Compiler -> String
getCompilerVersion (Compiler Gcc46   _ _ _ ) = "4.6"
getCompilerVersion (Compiler Gcc47   _ _ _ ) = "4.7"
getCompilerVersion (Compiler Gcc48   _ _ _ ) = "4.8"
getCompilerVersion (Compiler Gcc49   _ _ _ ) = "4.9"
getCompilerVersion (Compiler Clang31 _ _ _ ) = "3.1"
getCompilerVersion (Compiler Clang32 _ _ _ ) = "3.2"
getCompilerVersion (Compiler Clang33 _ _ _ ) = "3.3"
getCompilerVersion (Compiler Clang34 _ _ _ ) = "3.4"
getCompilerVersion (Compiler Clang35 _ _ _ ) = "3.5"
getCompilerVersion (Compiler Clang36 _ _ _ ) = "3.6"


getCompilerFamily :: Compiler -> CompilerFamily
getCompilerFamily (Compiler Gcc46   _ _ _ ) = Gcc
getCompilerFamily (Compiler Gcc47   _ _ _ ) = Gcc
getCompilerFamily (Compiler Gcc48   _ _ _ ) = Gcc
getCompilerFamily (Compiler Gcc49   _ _ _ ) = Gcc
getCompilerFamily (Compiler Clang31 _ _ _ ) = Clang
getCompilerFamily (Compiler Clang32 _ _ _ ) = Clang
getCompilerFamily (Compiler Clang33 _ _ _ ) = Clang
getCompilerFamily (Compiler Clang34 _ _ _ ) = Clang
getCompilerFamily (Compiler Clang35 _ _ _ ) = Clang
getCompilerFamily (Compiler Clang36 _ _ _ ) = Clang


getCompilerConf :: FilePath -> IO [Compiler]
getCompilerConf conf =
    doesFileExist conf >>= \b ->
        if b then read <$> readFile conf
             else return defaultCompilerList

getAvailCompilers :: [Compiler] -> IO [Compiler]
getAvailCompilers xs = do
    ys <- filterM (doesFileExist . compilerExec) xs
    zs <- filterM isValidCompiler ys
    ps <- mapM (canonicalizePath . compilerExec) zs
    let out =  map fst (nubBy (\(_,p1) (_,p2) -> p1 == p2) $ zip zs ps)
    return out


isValidCompiler :: Compiler -> IO Bool
isValidCompiler c =  (getCompilerVersion c `isPrefixOf`) <$> askCompilerVersion c


askCompilerVersion :: Compiler -> IO String
askCompilerVersion comp
    | Gcc <- getCompilerFamily comp = last . words . head . lines <$> readProcess (compilerExec comp) ["--version"] ""
    | otherwise                     = last . words . head . lines <$> readProcess (compilerExec comp) ["--version"] ""


getCompilerFamilyByName :: IO CompilerFamily
getCompilerFamilyByName = getProgName >>= \n ->
    return $ if n `isSuffixOf` "clang" then Clang else Gcc


compilerFilter :: CompilerFamily -> [Compiler] -> [Compiler]
compilerFilter t = filter $ (== t) . getCompilerFamily


compilerFilterType :: CompilerType -> [Compiler] -> [Compiler]
compilerFilterType t = filter $ (== t) . compilerType


getCompilerOpt :: Compiler -> [String]
getCompilerOpt (Compiler ver _ _ opts) =
        case ver of
         Gcc46   -> gcc_opt ++ opts
         Gcc47   -> gcc_opt ++ opts
         Gcc48   -> gcc_opt ++ opts
         Gcc49   -> gcc_opt ++ opts
         Clang31 -> clg_opt ++ opts
         Clang32 -> clg_opt ++ opts
         Clang33 -> clg_opt ++ opts
         Clang34 -> clg_opt ++ opts
         Clang35 -> clg_opt ++ opts
         Clang36 -> clg_opt ++ opts
    where gcc_opt = [ "-O0", "-D_GLIBCXX_DEBUG", "-pthread", "-Wall", "-Wextra", "-Wno-unused-parameter", "-Wno-unused-value" ]
          clg_opt = [ "-O0", "-D_GLIBCXX_DEBUG", "-pthread", "-Wall", "-Wextra", "-Wno-unused-parameter", "-Wno-unused-value", "-Wno-unneeded-internal-declaration"]


getCompilerOptPCH :: Compiler -> [String]
getCompilerOptPCH comp@(Compiler ver _ _ _) =
        case ver of
         Gcc46   -> getCompilerOpt comp ++ [ "-Winvalid-pch", "-I" ++ includeAssDir </> "4.6" ]  ++ ["-I" ++ includeAssDir ]
         Gcc47   -> getCompilerOpt comp ++ [ "-Winvalid-pch", "-I" ++ includeAssDir </> "4.7" ]  ++ ["-I" ++ includeAssDir ]
         Gcc48   -> getCompilerOpt comp ++ [ "-Winvalid-pch", "-I" ++ includeAssDir </> "4.8" ]  ++ ["-I" ++ includeAssDir ]
         Gcc49   -> getCompilerOpt comp ++ [ "-Winvalid-pch", "-I" ++ includeAssDir </> "4.9" ]  ++ ["-I" ++ includeAssDir ]
         Clang31 -> getCompilerOpt comp ++ ["-include ", getCompilerPchPath comp </> "ass.hpp" ] ++ ["-I" ++ includeAssDir ]
         Clang32 -> getCompilerOpt comp ++ ["-include ", getCompilerPchPath comp </> "ass.hpp" ] ++ ["-I" ++ includeAssDir ]
         Clang33 -> getCompilerOpt comp ++ ["-include ", getCompilerPchPath comp </> "ass.hpp" ] ++ ["-I" ++ includeAssDir ]
         Clang34 -> getCompilerOpt comp ++ ["-include ", getCompilerPchPath comp </> "ass.hpp" ] ++ ["-I" ++ includeAssDir ]
         Clang35 -> getCompilerOpt comp ++ ["-include ", getCompilerPchPath comp </> "ass.hpp" ] ++ ["-I" ++ includeAssDir ]
         Clang36 -> getCompilerOpt comp ++ ["-include ", getCompilerPchPath comp </> "ass.hpp" ] ++ ["-I" ++ includeAssDir ]


getCompilerPchPath ::  Compiler -> FilePath
getCompilerPchPath (Compiler ver _ _ opts) =
    case ver of
         Gcc46   -> includeAssDir </> "4.6"
         Gcc47   -> includeAssDir </> "4.7"
         Gcc48   -> includeAssDir </> "4.8"
         Gcc49   -> includeAssDir </> "4.9"
         Clang31 -> includeAssDir </> "clang31" </> clangDir
         Clang32 -> includeAssDir </> "clang32" </> clangDir
         Clang33 -> includeAssDir </> "clang33" </> clangDir
         Clang34 -> includeAssDir </> "clang34" </> clangDir
         Clang35 -> includeAssDir </> "clang35" </> clangDir
         Clang36 -> includeAssDir </> "clang36" </> clangDir
    where
        clangDir |  "-std=c++1y" `elem` opts && "-stdlib=libc++" `elem` opts = "libc++1y"
                 |  "-stdlib=libc++" `elem` opts                             = "libc++"
                 |  otherwise                                                = "glibcxx"


runCompiler :: Compiler -> [FilePath] -> FilePath -> Bool -> [String] -> IO ExitCode
runCompiler cxx sources binary verbose user_opt =
    when verbose (putStrLn cmd) >> system cmd
        where cmd = unwords . concat $ [[compilerExec cxx], getCompilerOptPCH cxx, user_opt, sources, ["-o"], [binary]]


