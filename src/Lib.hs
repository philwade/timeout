module Lib
    ( uncomment
    , comment
    , createEntryLine
    , isEntry
    , getName
    , transformNamedEntries
    , transformAllEntries
    , uncommentAll
    , commentAll
    , addEntry
    , HostFile
    , ChangeType (..)
    , getChangeFromArgs
    ) where

import Data.List
import Text.Regex.Posix

type HostEntry = String
type HostFile = [HostEntry]

uncomment :: HostEntry -> HostEntry
uncomment (x:xs) =
    case x of
        '#' -> uncomment xs
        _ -> x:xs
uncomment line = line

comment :: HostEntry -> HostEntry
comment (x:xs) =
    case x of
        '#' -> x:xs
        _ -> '#':x:xs
comment "" = ""

createEntryLine :: String -> [String] -> HostEntry
createEntryLine name urls =
    "127.0.0.1 " ++ (intercalate " " urls) ++ " #timeout:" ++ name

pattern :: String
pattern = "#timeout:(.*)$"

isEntry :: HostEntry -> Bool
isEntry line = line =~ pattern :: Bool

getName :: String -> Maybe String
getName line =
    let
        values = line =~ pattern :: [[String]]
    in
        case values of
            ((_:z:_):_) -> Just z
            _ -> Nothing

transformNamedEntries :: (HostEntry -> HostEntry) -> [String] -> HostFile -> HostFile
transformNamedEntries transform names filecontents =
    map (\line ->
            case isEntry line of
                True -> case getName line of
                            Just a ->
                                if a `elem` names then
                                    transform line
                                else
                                    line
                            _ -> line
                False -> line) filecontents

transformAllEntries :: (HostEntry -> HostEntry) -> HostFile -> HostFile
transformAllEntries transform filecontents =
    map (\line ->
            case isEntry line of
                True -> transform line
                False -> line) filecontents

uncommentAll :: HostFile -> HostFile
uncommentAll = transformAllEntries uncomment

commentAll :: HostFile -> HostFile
commentAll = transformAllEntries comment

addEntry :: String -> [String] -> HostFile -> HostFile
addEntry name urls hosts =
    hosts ++ [(createEntryLine name urls)]

data ChangeType = On
                | Off
                | TargetedOn [String]
                | TargetedOff [String]
                | AddEntry String [String]
                deriving (Show, Eq)

getChangeFromArgs :: [String] -> ChangeType
getChangeFromArgs args =
    case args of
        ["out"] -> Off
        ["in"] -> On
        "in":names -> TargetedOn names
        "out":names -> TargetedOff names
        "add":name:urls -> AddEntry name urls
        [] -> Off
        _ -> error "Unknown arguments"
