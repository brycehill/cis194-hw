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
insert msg1@(LogMessage _ _ _) tree@(Node _ msg2@(LogMessage _ _ _) _)
   | greaterTS msg2 msg1 = Node Leaf msg1 tree
   | greaterTS msg1 msg2 = Node tree msg1 Leaf
insert (Unknown _) tree = tree


greaterTS :: LogMessage -> LogMessage -> Bool
greaterTS (LogMessage _ ts _) (LogMessage _ ts2 _) = ts > ts2


build :: [LogMessage] -> MessageTree
build [] = Leaf
build [x] = insert x Leaf
build (x:ys) = insert x (build ys)


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf l Leaf) = [l]
inOrder (Node Leaf l tree@(Node _ _ _)) = inOrder tree ++ [l]
inOrder (Node tree@(Node _ _ _) l Leaf) = l : inOrder tree


getString :: LogMessage -> String
getString (LogMessage _ _ s) = s
getString (Unknown s) = s


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getString . filter gt50
  where
    gt50 :: LogMessage -> Bool
    gt50 (LogMessage (Error x) _ _) = x > 50
    gt50 _ = False
