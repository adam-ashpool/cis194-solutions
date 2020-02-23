{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import           Log

parseMessage :: String -> LogMessage
parseMessage msg =
  case (words msg) of
    ("I":ts:context) -> LogMessage Info (read ts) (unwords context)
    ("W":ts:context) -> LogMessage Warning (read ts) (unwords context)
    ("E":severity:ts:context) ->
      LogMessage (Error (read severity)) (read ts) (unwords context)
    _ -> Unknown msg

parse :: String -> [LogMessage]
parse msgs = map parseMessage $ lines msgs

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert msg Leaf = Node Leaf msg Leaf
insert (msg@(LogMessage _ ts _)) (Node ltree (nodeMsg@(LogMessage _ nodeTs _)) rtree) =
  if ts < nodeTs
    then Node (insert msg ltree) nodeMsg rtree
    else Node ltree nodeMsg (insert msg rtree)
insert _ _ = error "Tree with Unknown element"

build :: [LogMessage] -> MessageTree
build msgs = foldr (\msg acc -> insert msg acc) Leaf msgs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                   = []
inOrder (Node ltree msg rtree) = (inOrder ltree) ++ [msg] ++ (inOrder rtree)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong msgs =
  let isSevereError (LogMessage (Error severity) _ _) = severity >= 50
      isSevereError _                                 = False
   in (map (\(LogMessage _ _ context) -> context) .
       filter (isSevereError) . inOrder . build)
        msgs
