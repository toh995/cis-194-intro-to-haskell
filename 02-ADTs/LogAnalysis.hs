{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Text.Read (readMaybe)
import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage = (parseWordList.words)

parseWordList :: [String] -> LogMessage
parseWordList ws@("I":time:rest)      = case (readMaybe time) of
                                             (Just t)  -> LogMessage Info t (unwords rest)
                                             (Nothing) -> Unknown (unwords ws)
parseWordList ws@("W":time:rest)      = case (readMaybe time) of
                                             (Just t)  -> LogMessage Warning t (unwords rest)
                                             (Nothing) -> Unknown (unwords ws)
parseWordList ws@("E":code:time:rest) = case (readMaybe code, readMaybe time) of
                                             (Just c, Just t) -> LogMessage (Error c) t (unwords rest)
                                             (Nothing, _)     -> Unknown (unwords ws)
                                             (_, Nothing)     -> Unknown (unwords ws)
parseWordList ws                      = Unknown (unwords ws)


parse :: String -> [LogMessage]
parse = (map parseMessage).lines

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _)               tree                              = tree
insert newLog@(LogMessage _ _ _) Leaf                              = Node Leaf newLog Leaf
insert newLog@(LogMessage _ _ _) (Node leftTree currLog rightTree)
  | currLog `gt` newLog = Node (insert newLog leftTree) currLog rightTree
  | otherwise           = Node leftTree                 currLog (insert newLog rightTree)

gt :: LogMessage -> LogMessage -> Bool
gt (Unknown _)         _                   = False
gt _                   (Unknown _)         = False
gt (LogMessage _ t1 _) (LogMessage _ t2 _) = t1 > t2

-- Exercise 3
build :: [LogMessage] -> MessageTree
build []     = Leaf
build (x:xs) = (insert x (build xs))

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                        = []
inOrder (Node leftTree l rightTree) = (inOrder leftTree)
                                      ++ [l]
                                      ++ (inOrder rightTree)

sortLogs :: [LogMessage] -> [LogMessage]
sortLogs = inOrder.build

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map getLogBody).sortLogs.(filter isSevereError)

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error code) _ _) = code > 50
isSevereError _                             = False

getLogBody :: LogMessage -> String
getLogBody (LogMessage _ _ b) = b
getLogBody _                  = ""
