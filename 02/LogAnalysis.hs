{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- | Exercise 1

-- Parse given error log lines

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

-- Parse message data from an error log line

parseMessage :: String -> LogMessage
parseMessage ln = case logLine of
                    ("I":time:msg)     -> LogMessage (Info)              (toInt time) (unwords msg)
                    ("W":time:msg)     -> LogMessage (Warning)           (toInt time) (unwords msg)
                    ("E":lvl:time:msg) -> LogMessage (Error (toInt lvl)) (toInt time) (unwords msg)
                    message            -> Unknown $ unwords message
  where logLine = words ln
        toInt s = read $ s :: Int

-- | Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert m@(LogMessage _ msgTime _) (Node left nodeM@(LogMessage _ nodeTime _) right)
  | msgTime < nodeTime = Node (insert m left) nodeM right
  | msgTime > nodeTime = Node left nodeM (insert m right)
  | otherwise          = error "insert: message has same timestamp as a node"
insert _ (Node _ (Unknown _) _) = error "insert: tree contains Unknown"

-- | Exercise 3

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

-- | Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right

-- | Exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = messages . filterSeverity . sortByTime . getErrors
  where getErrors = filter isError
        sortByTime = inOrder . build
        filterSeverity = filter isSevereError
        messages = map (\(LogMessage _ _ msg) -> msg)

isError :: LogMessage -> Bool
isError (LogMessage (Error _) _ _) = True
isError _                          = False

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error severity) _ _) = severity > limit
  where limit = 49
isSevereError _ = False

