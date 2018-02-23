module Lib
    ( uncomment
    , comment
    , createEntryLine
    , isEntry
    , getName
    ) where

import Data.List
import Text.Regex.Posix

uncomment :: String -> String
uncomment (x:xs) =
    case x of
        '#' -> uncomment xs
        _ -> x:xs
uncomment line = line

comment :: String -> String
comment (x:xs) =
    case x of
        '#' -> x:xs
        _ -> '#':x:xs
comment "" = ""

createEntryLine :: String -> [String] -> String
createEntryLine name urls =
    "127.0.0.1 " ++ (intercalate " " urls) ++ " #timeout:" ++ name

pattern :: String
pattern = "#timeout:(.*)$"

isEntry :: String -> Bool
isEntry line = line =~ pattern :: Bool

getName :: String -> Maybe String
getName line =
    let
        values = line =~ pattern :: [[String]]
    in
        case values of
            ((_:z:_):_) -> Just z
            _ -> Nothing
