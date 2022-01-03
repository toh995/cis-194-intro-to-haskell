{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Data.Char (isDigit)

import Log

-- Exercise 1 - PARSING
parse :: String -> [LogMessage]
parse = map parseMessage . lines

parseMessage :: String -> LogMessage
parseMessage msg@('I':_) = parseInfoMsg msg
parseMessage msg@('W':_) = parseWarningMsg msg
parseMessage msg@('E':_) = parseErrorMsg msg
parseMessage msg         = Unknown msg


parseInfoMsg :: String -> LogMessage
parseInfoMsg msg = case words msg of
                     ("I":timestamp:lastWords) -> if all isDigit timestamp
                                                    then LogMessage
                                                      Info
                                                      (read timestamp :: TimeStamp)
                                                      (unwords lastWords)
                                                  else Unknown msg
                     _                         -> Unknown msg

parseWarningMsg :: String -> LogMessage
parseWarningMsg msg = case words msg of
                        ("W":timestamp:lastWords) -> if all isDigit timestamp
                                                       then LogMessage
                                                         Info
                                                         (read timestamp :: TimeStamp)
                                                         (unwords lastWords)
                                                     else Unknown msg
                        _                         -> Unknown msg

parseErrorMsg :: String -> LogMessage
parseErrorMsg msg = case words msg of
                      ("E":errCode:timestamp:lastWords) -> if all isDigit errCode && all isDigit timestamp
                                                             then LogMessage
                                                               (Error (read errCode :: Int))
                                                               (read timestamp :: TimeStamp)
                                                               (unwords lastWords)
                                                           else Unknown msg
                      _                                 -> Unknown msg

-- Exercise 2 - INSERT INTO BST
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msgToInsert Leaf = Node Leaf msgToInsert Leaf
insert msgToInsert currTree@(Node lTree currMsg rTree)
  | msgToInsert `greaterThan` currMsg = Node lTree currMsg (insert msgToInsert rTree)
  | msgToInsert `lessThan` currMsg    = Node (insert msgToInsert lTree) currMsg rTree
  | otherwise                         = currTree

greaterThan :: LogMessage -> LogMessage -> Bool
greaterThan (Unknown _) _                           = False
greaterThan _ (Unknown _)                           = False
greaterThan (LogMessage _ t1 _) (LogMessage _ t2 _) = t1 > t2

lessThan :: LogMessage -> LogMessage -> Bool
lessThan (Unknown _) _                           = False
lessThan _ (Unknown _)                           = False
lessThan (LogMessage _ t1 _) (LogMessage _ t2 _) = t1 < t2

-- Exercise 3 - BUILD BST
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4 - TREE TO LIST
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lTree msg rTree) = inOrder lTree ++ [msg] ++ inOrder rTree

-- Exercise 5 - SORT AND FILTER A LIST OF LOG MSGS
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMsgBody . inOrder . build . filter (moreSevereThan 50)

moreSevereThan :: Int -> LogMessage -> Bool
moreSevereThan threshold (LogMessage (Error severity) _ _) = severity > threshold
moreSevereThan _ _                                         = False

getMsgBody :: LogMessage -> String
getMsgBody (LogMessage _ _ body) = body
getMsgBody (Unknown body)        = body
