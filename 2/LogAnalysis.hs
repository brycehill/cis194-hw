{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

data LogMessageType = MessageType | TimeStamp | String


parseMessage :: String -> LogMessage
parseMessage s =
  case words s of
    ("I":y:zs) -> LogMessage Info (read y) (unwords zs)
    ("W":y:zs) -> LogMessage Warning (read y) (unwords zs)
    ("E":y:zs) -> LogMessage (Error (read y)) (readHead zs) (unwords $ tail zs)
    _ -> Unknown s

readHead :: [String] -> Int
readHead = read . unwords . take 1

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)
