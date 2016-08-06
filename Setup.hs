import Distribution.Simple
import Distribution.Simple.Setup(InstallFlags(..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.PackageDescription (PackageDescription(..))

import System.Environment
import System.Directory
import System.FilePath.Posix
import System.Process
import System.Directory
import System.FilePath
import System.Posix.User
import System.Environment
import System.Exit

import Control.Monad

includeDir = "/usr/local/include/ass"
rcfile = ".assrc"

main = defaultMainWithHooks $
        simpleUserHooks
        {
            postInst = (installRC >> installHeaders)
        }


installHeaders :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
installHeaders _ _ _ _ = do
    putStrLn "Copying headers..."
    createDirectoryIfMissing    True includeDir
    copyFile "includes/ass.hpp"         (includeDir </> "ass.hpp")
    copyFile "includes/ass-boost.hpp"   (includeDir </> "ass-boost.hpp")
    copyFile "includes/ass-cat.hpp"     (includeDir </> "ass-cat.hpp")
    putStrLn "Done."
                    
                    
installRC :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
installRC _ _ _ _ = do
    putStrLn $ "Copying " ++ rcfile ++ "..."
    dest <- liftM (</> rcfile) getHomeDirectory
    doesFileExist dest >>= \n ->
        if n then putStrLn $ "~/" ++ rcfile ++ " already installed."
             else copyFile rcfile dest


