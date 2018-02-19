{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( main
    ) where

import System.Cron.Schedule

main :: IO ()
main = do
    tids <- execSchedule $ do
        addJob job1 "* * * * *"
    print tids

job1 :: IO ()
job1 = putStrLn "heyoheyo"
