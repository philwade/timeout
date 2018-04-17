module Main where

import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import Data.List (intercalate)
import System.Environment
import System.Posix.Files
import System.Console.ANSI
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
                        let change = getChangeFromArgs args
                        let newhosts = unlines $ updateHosts change (lines hostcontent)
                        writeFile hostsFile newhosts
                        setConsoleColor change
                        displayChange change
                else
                    do
                    setSGR [SetColor Foreground Vivid Red]
                    putStrLn $ "Please run with sudo so I can modify " ++ hostsFile
        setSGR [Reset]

setConsoleColor :: ChangeType -> IO ()
setConsoleColor change =
    case change of
        On -> setSGR [SetColor Foreground Vivid Red]
        Off -> setSGR [SetColor Foreground Vivid Green]
        TargetedOff _ -> setSGR [SetColor Foreground Vivid Green]
        TargetedOn _ -> setSGR [SetColor Foreground Vivid Red]
        _ -> return ()

displayChange :: ChangeType -> IO ()
displayChange change =
    case change of
        On -> putStrLn "Unblocked all sites"
        Off -> putStrLn "Blocked all sites"
        TargetedOff names -> putStrLn $ "Blocked sites: " ++ (intercalate ", " names)
        TargetedOn names -> putStrLn $ "Unblocked sites: " ++ (intercalate ", " names)
        AddEntry name urls -> putStrLn $ "Added site " ++ name ++ " with urls " ++ (intercalate ", " urls) ++ " to blocking"

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

updateHosts :: ChangeType -> HostFile -> HostFile
updateHosts change hosts =
    case change of
        Off -> commentAll hosts
        On -> uncommentAll hosts
        TargetedOn names -> transformNamedEntries uncomment names hosts
        TargetedOff names -> transformNamedEntries comment names hosts
        AddEntry name urls -> addEntry name urls hosts
