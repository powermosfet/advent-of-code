#!/bin/env stack
{- stack
   script
   --resolver lts-16.24
   --package turtle
   --package text
 -}
{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Turtle.Shell (reduce)
import qualified Data.Text as Text

data Terrain = Ground | Tree
  deriving (Show)

parseLocation '.' = Ground
parseLocation '#' = Tree

step right (index, treeCount) terrain =
  let
    currentSpot = terrain !! index
    nextIndex = index + right
  in
  case currentSpot of
    Ground -> (nextIndex, treeCount)
    Tree -> (nextIndex, treeCount + 1)

initial = (0, 0)

extract (_, treeCount) = treeCount

folder right = Fold (step right) initial extract

main = do
  result1 <- (reduce (folder 1) $ do
    contents <- input "2020/03/input.txt"
    return $ cycle $ map parseLocation $ Text.unpack $ lineToText contents
    )
  print result1
  result2 <- (reduce (folder 3) $ do
    contents <- input "2020/03/input.txt"
    return $ cycle $ map parseLocation $ Text.unpack $ lineToText contents
    )
  print result2
  result3 <- (reduce (folder 5) $ do
    contents <- input "2020/03/input.txt"
    return $ cycle $ map parseLocation $ Text.unpack $ lineToText contents
    )
  print result3
  result4 <- (reduce (folder 7) $ do
    contents <- input "2020/03/input.txt"
    return $ cycle $ map parseLocation $ Text.unpack $ lineToText contents
    )
  print result4
  result5 <- (reduce (folder 1) $ do
    contents <- input "2020/03/input_2.txt"
    return $ cycle $ map parseLocation $ Text.unpack $ lineToText contents
    )
  print result5
  echo "Done!"
