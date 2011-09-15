-- ass: C++ code ass'istant

import Char
import System(getArgs)
import System.Process
import System.IO

data CodeLine = CodeLine Int String 

type TranslationUnit = [CodeLine]
type MainFunction    = [CodeLine]
type LineNumber      = Int

instance Show CodeLine where
    show (CodeLine n s) = "#line " ++ show n ++ "\n" ++ s  

isPreprocessor :: String -> Bool
isPreprocessor [] = False
isPreprocessor (x:xs) 
   | isSpace x = isPreprocessor xs
   | x == '#'  = True
   | otherwise = False

isSep :: String -> Bool
isSep xs = (spaces + dots) == (length xs) && (dots > 2)
            where
                spaces = length $ filter isSpace xs
                dots   = length $ filter ( == '.') xs 

getCompilerArgs :: [String] -> [String]
getCompilerArgs = takeWhile ( /= "--" )  

tail' :: [String] -> [String]
tail' [] = []
tail' (x:xs) = xs

getTestArgs :: [String] -> [String]
getTestArgs = tail' . dropWhile ( /= "--" ) 

parseSource' :: TranslationUnit -> MainFunction ->  Bool -> [CodeLine] -> (TranslationUnit, MainFunction)
parseSource' t m  _  [] =  (t, m ++ [ CodeLine 0 "}" ])
parseSource' t m state (CodeLine n x:xs)
     | isSep x          =   parseSource' t m (not state) xs
     | isPreprocessor x =   parseSource' (t ++ [CodeLine n x] ) m state xs
     | otherwise        =   if state
                             then parseSource' (t ++ [CodeLine n x]) m state xs
                             else parseSource' t (m ++ [CodeLine n x]) state xs
 
parseSource :: TranslationUnit -> MainFunction -> [CodeLine] -> (TranslationUnit, MainFunction)
parseSource t m code = parseSource' t m False code

toCodeLine :: String -> IO [CodeLine]
toCodeLine xs = return (map (\ (xs,n) -> CodeLine n xs) $ zip (lines xs) [1..])

main = do
    args <- getArgs
    
    let translation = [ CodeLine 0 "#include <ass.hpp>" ]
    let main = [ CodeLine 0 "int main(int argc, char *argv[]) { cout << boolalpha;" ]
    let compileCmd = "/usr/bin/g++ -std=c++0x -Wall -Wextra -Wno-unused-parameter " 
                        ++ "-D_GLIBXX_DEBUG /tmp/runme.cpp -o /tmp/runme "  ++ ( unwords $ getCompilerArgs args )
    let testCmd = "/tmp/runme " ++ ( unwords $ getTestArgs args ) 

    -- parse the snippet.

    source <- hGetContents stdin >>= toCodeLine
    let (translation',main') = parseSource translation main source

    -- create source code.
    handle <- openFile "/tmp/runme.cpp" WriteMode
    mapM (hPrint handle ) translation'
    mapM (hPrint handle ) main'
    hClose handle
    
    -- compile and run it.
    rc <- runCommand compileCmd >>= waitForProcess >> runCommand testCmd >>= waitForProcess
    return rc

