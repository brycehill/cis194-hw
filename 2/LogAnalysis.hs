{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

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

insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = Node Leaf msg Leaf
insert msg1 tree@(Node _ msg2@(LogMessage{}) _)
   | greaterTS msg2 msg1 = Node Leaf msg1 tree
   | otherwise           = Node tree msg1 Leaf
insert (Unknown _) tree = tree


greaterTS :: LogMessage -> LogMessage -> Bool
greaterTS (LogMessage _ ts _) (LogMessage _ ts2 _) = ts > ts2


build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf msg Leaf) = [msg]
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right


getString :: LogMessage -> String
getString (LogMessage _ _ s) = s
getString (Unknown s) = s


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getString . inOrder . build . filter (errorGT 50)
  where
    errorGT :: Int -> LogMessage -> Bool
    errorGT n (LogMessage (Error x) _ _) = x > n
    errorGT _ _ = False
