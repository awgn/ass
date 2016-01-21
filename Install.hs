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


import Ass.Config
import Ass.Compiler

import System.Process
import System.Directory
import System.FilePath
import System.Posix.User
import System.Console.ANSI
import System.Environment
import System.Exit

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
        doesFileExist dest >>= \n ->
            if n then putMsg $ "~/" ++ assrc ++ " already installed."
                 else copyFile assrc dest


installHdr :: IO ()
installHdr = do
        putMsg "Copying headers..."
        createDirectoryIfMissing True includeAssDir
        copyFile "includes/ass.hpp"  (includeAssDir </> "ass.hpp")
        copyFile "includes/ass-boost.hpp"   (includeAssDir </> "ass-boost.hpp")


installVimPlugin :: IO ()
installVimPlugin = do
        bundle <- liftM (</> ".vim" </> "bundle") getHomeDirectory
        doesDirectoryExist bundle >>= \n ->
            when n $ do putMsg "Installing vim-cpp-ass plungin (pathogen/vundle plug-in detected)..."
                        createDirectoryIfMissing False (bundle </> "vim-cpp-ass")
                        copyFile "plugin/ass.vim" (bundle </> "vim-cpp-ass/ass.vim")


installBinaries :: IO ()
installBinaries = do
        putMsg "Compiling haskell binaries..."

        void $ system "runhaskell Setup configure --user"
        void $ system "runhaskell Setup build"
        void $ system "runhaskell Setup install"

        copyFile "dist/build/ass/ass" (installDir </> "ass")
        copyFile "dist/build/ass/ass" (installDir </> "ass-clang")
        copyFile "dist/build/gen/gen" (installDir </> "gen")


getPchExtension :: Compiler -> String
getPchExtension (Compiler typ _ _ _ _) =
        if typ `elem` [Gcc46, Gcc47, Gcc48, Gcc49, Gcc5]
            then "gch"
            else "pch"


installPch:: IO ()
installPch = do
        home  <- getHomeDirectory

        putMsg "Compiling PCH headers:"
        putMsg $ "Getting compilers configuration from " ++ home </> assrc ++ "..."

        list  <- getCompilerConf (home </> assrc) >>= getAvailCompilers

        void $ forM_ list $ \comp ->
            putMsg $ "Installing pch for " ++ compilerName comp ++ "..."

        void $ flip mapConcurrently list $ \comp -> do

            let pchDir = getCompilerPchPath comp
            let opts   = getCompilerOpt comp

            createDirectoryIfMissing True pchDir
            void $ system $ compilerExec comp ++ " includes/ass.hpp " ++ unwords opts ++ " -o " ++ pchDir </> "ass.hpp." ++ getPchExtension comp
            doesDirectoryExist "/usr/include/boost" >>= \boost ->
                when boost $ void $ system $ compilerExec comp ++ " includes/ass-boost.hpp " ++ unwords opts ++ " -o " ++ pchDir </> "ass-boost.hpp." ++ getPchExtension comp


usage :: IO ()
usage = putStrLn "Install.hs [--help][--ass][--vim-plugin][--pch][--all]"


main = do
    args <- getArgs

    when (null args || "--help" `elem` args || "-h" `elem` args || "-?" `elem` args)
        $ usage >> void exitSuccess

    putMsg "Installing C++11/14 assistant."

    sequence . concat $
        [
            [installrc        | hasOpt "--ass" args],
            [installBinaries  | hasOpt "--ass" args],
            [installHdr       | hasOpt "--ass" args],
            [installVimPlugin | hasOpt "--vim-plugins" args],
            [installPch       | hasOpt "--phc" args]
        ]

    putMsg "done."
        where hasOpt x xs = x `elem` xs || "--all" `elem` xs

