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
        ["out"] -> uncommentAll hosts
        ["in"] -> commentAll hosts
        "in":names -> transformNamedEntries comment names hosts
        "out":names -> transformNamedEntries uncomment names hosts
        "add":name:urls -> addEntry name urls hosts
        [] -> uncommentAll hosts
        _ -> hosts
