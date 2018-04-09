module Main where

import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import System.Environment
import Lib

hostsFile :: String
hostsFile = "samplehosts"

main :: IO ()
main = do
        hostcontent <- readFile hostsFile
        args <- getArgs
        let newhosts = unlines $ updateHosts args (lines hostcontent)
        putStrLn newhosts
        writeFile hostsFile newhosts

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
