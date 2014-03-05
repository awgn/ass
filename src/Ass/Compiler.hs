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

import Ass.Types

import System.Directory
import System.Process
import System.Environment

import Control.Monad
import Control.Applicative

import Data.List
import Config


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
validCompilers = filterM (\c -> (getCompilerVersion c `isPrefixOf`) <$> askCompilerVersion c)


askCompilerVersion :: Compiler -> IO String
askCompilerVersion comp
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

