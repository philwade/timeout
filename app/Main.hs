module Main where

import System.IO
import System.Environment
import Lib

main :: IO ()
main = do
        hosts <- openFile "samplehosts" ReadMode
        hostcontent <- hGetContents hosts
        args <- getArgs
        putStrLn $ unlines $ updateHosts args (lines hostcontent)

updateHosts :: [String] -> HostFile -> HostFile
updateHosts args hosts =
    case args of
        ["out"] -> commentAll hosts
        ["in"] -> uncommentAll hosts
        "in":names -> transformNamedEntries uncomment names hosts
        "out":names -> transformNamedEntries comment names hosts
        _ -> hosts
