module Main where

import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import System.Environment
import System.Posix.Files
import Lib

hostsFile :: String
hostsFile = "/etc/hosts"

main :: IO ()
main = do
        haveAccess <- fileAccess hostsFile False True False
        args <- getArgs
        case args of
            ["help"] -> printUsage
            ["-h"] -> printUsage
            _ ->
                if haveAccess then
                        do
                        hostcontent <- readFile hostsFile
                        let newhosts = unlines $ updateHosts args (lines hostcontent)
                        putStrLn newhosts
                        writeFile hostsFile newhosts
                else
                    putStrLn $ "Please run with sudo so I can modify " ++ hostsFile

printUsage :: IO ()
printUsage =
    do
        putStrLn "timeout - Block yourself from unproductive websites"
        putStrLn ""
        putStrLn "          timeout uses your host file to block access to websites, so it requires sudo permissions to run."
        putStrLn ""
        putStrLn "Usage: timeout [COMMAND] [arguments]"
        putStrLn ""
        putStrLn "Available commands:"
        putStrLn "  out                                   Block configured websites (default without any command)"
        putStrLn "  out [sitename]                        Block [sitename] configured websites"
        putStrLn "  in                                    Unblock configured websites"
        putStrLn "  in [sitename] [sitename]...           Unblock [sitename] configured websites"
        putStrLn "  add [sitename] [urls]...              Create block entry named [sitename] composed of [urls]"
        putStrLn ""
        putStrLn ""
        putStrLn "Examples:"
        putStrLn ""
        putStrLn "Add twitter to block list and block:"
        putStrLn "  $ timeout add twitter twitter.com www.twitter.com"
        putStrLn ""
        putStrLn "Block all sites configured:"
        putStrLn "  $ timeout"
        putStrLn ""
        putStrLn "Unblock all sites configured:"
        putStrLn "  $ timeout in"
        putStrLn ""
        putStrLn "Unblock just twitter:"
        putStrLn "  $ timeout in twitter"
        putStrLn ""
        putStrLn "Unblock twitter and facebook:"
        putStrLn "  $ timeout in twitter facebook"

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
