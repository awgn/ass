--
-- Copyright (c) 2014 Bonelli Nicola <bonelli@antifork.org>
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

{-# LANGUAGE LambdaCase #-}

import Config
import Ass.Compiler

import System.Process
import System.Directory
import System.FilePath
import System.Posix.User
import System.Console.ANSI

import Control.Monad
import Control.Concurrent.Async

bold    = setSGRCode [SetConsoleIntensity BoldIntensity]
reset   = setSGRCode []

putMsg :: String -> IO ()
putMsg msg = putStrLn $ bold ++ "[ass] "++ reset ++ msg


installrc :: IO ()
installrc = do
        putMsg $ "Copying " ++ assrc ++ "..."
        dest <- liftM (</> assrc) getHomeDirectory
        doesFileExist dest >>= \case
                True  -> putMsg $ "~/" ++ assrc ++ " already installed."
                False -> copyFile (".." </> assrc) dest


installHdr :: IO ()
installHdr = do
        putMsg "Copying headers..."
        createDirectoryIfMissing True includeAssDir
        copyFile ("../includes/ass.hpp")         (includeAssDir </> "ass.hpp")
        copyFile ("../includes/ass-boost.hpp")   (includeAssDir </> "ass-boost.hpp")


installVimPlugin :: IO ()
installVimPlugin = do
        bundle <- liftM (</> ".vim" </> "bundle") getHomeDirectory
        doesDirectoryExist bundle >>= \case
                    True  -> do
                                putMsg "Installing vim-ass plungin (pathong detected)..."
                                createDirectoryIfMissing False (bundle </> "vim-ass")
                                copyFile (".." </> "plugin/ass.vim") (bundle </> "vim-ass/ass.vim")
                    False -> return ()


installBinaries :: IO ()
installBinaries = do
        putMsg "Compiling haskell binaries..."
        void $ system ("/usr/bin/ghc --make -O -Wall Ass.hs -o " ++ (installDir </> "ass"))
        void $ system ("/usr/bin/ghc --make -O -Wall Ass.hs -o " ++ (installDir </> "ass-clang"))
        void $ system ("/usr/bin/ghc --make -O -Wall Gen.hs -o " ++ (installDir </> "gen"))


getPchExtension :: Compiler -> String

getPchExtension (Compiler typ _ _ _) =
        if typ `elem` [Gcc46, Gcc47, Gcc48, Gcc49]
            then "gch"
            else "pch"

installPch:: IO ()
installPch = do
        putMsg "PCH headers..."
        home  <- getHomeDirectory
        list  <- getCompilerConf (home </> assrc) >>= getAvailCompilers >>= getValidCompilers

        void $ flip mapConcurrently list $ \comp -> do
            putMsg $ "Installing pch for " ++ (compilerName comp) ++ "..."

            let pchDir = getCompilerPchPath comp
            let opts   = getCompilerOpt comp

            createDirectoryIfMissing True pchDir
            void $ system $ compilerExec comp ++ " ../includes/ass.hpp " ++ unwords opts ++ " -o " ++ pchDir </> "ass.hpp." ++ getPchExtension comp
            doesDirectoryExist "/usr/include/boost" >>= \boost ->
                when boost $ void $ system $ compilerExec comp ++ " ../includes/ass-boost.hpp " ++ unwords opts ++ " -o " ++ pchDir </> "ass-boost.hpp." ++ getPchExtension comp
main = do
    putMsg "Installing C++11/14 assistant."
    installVimPlugin
    installrc
    installHdr
    installBinaries
    installPch
    putMsg "done."
