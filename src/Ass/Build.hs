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


module Ass.Build where

import Ass.Config
import Ass.Compiler

import System.Process
import System.Directory
import System.FilePath

import Control.Monad
import Control.Concurrent.Async


getPchExtension :: Compiler -> String
getPchExtension (Compiler typ _ _ _ _) =
        if typ `elem` [Gcc46, Gcc47, Gcc48, Gcc49, Gcc5, Gcc6]
            then "gch"
            else "pch"


buildPCH:: IO ()
buildPCH = do
    home  <- getHomeDirectory

    putStrLn "Compiling PCH headers:"
    putStrLn $ "Getting compilers configuration from " ++ home </> assrc ++ "..."

    list  <- getCompilerConf (home </> assrc) >>= getAvailCompilers

    void $ forM_ list $ \comp ->
        putStrLn $ "Installing pch for " ++ compilerName comp ++ "..."

    void $ flip mapConcurrently list $ \comp -> do

        let pchDir = getCompilerPchPath comp
        let opts   = getCompilerOpt comp

        createDirectoryIfMissing True pchDir
        void $ system $ compilerExec comp ++ " /usr/local/include/ass/ass.hpp " ++ unwords opts ++ " -o " ++ pchDir </> "ass.hpp." ++ getPchExtension comp

        doesDirectoryExist "/usr/include/boost" >>= \boost ->
            when boost $ void $ system $ compilerExec comp ++ " /usr/local/include/ass/ass-boost.hpp " ++ unwords opts ++ " -o " ++ pchDir </> "ass-boost.hpp." ++ getPchExtension comp

        doesDirectoryExist "/usr/local/include/cat" >>= \cat ->
            when cat $ void $ system $ compilerExec comp ++ " /usr/local/include/ass/ass-cat.hpp " ++ unwords opts ++ " -o " ++ pchDir </> "ass-cat.hpp." ++ getPchExtension comp

        
