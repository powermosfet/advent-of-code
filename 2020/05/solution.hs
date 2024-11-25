#!/bin/env stack
{- stack
   script
   --resolver lts-16.24
   --package containers
   --package text
   --package parsec
   --package foldl
 -}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import Text.ParserCombinators.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Char as Char
import qualified Control.Foldl as Fold
import qualified Data.Map as Map
import Text.Read
import GHC.List (elem)
import Data.List
import Data.Maybe

data Longitudinal = F | B
  deriving (Show, Read)

data Horizontal = L | R
  deriving (Show, Read)

lFact :: Longitudinal -> Int
lFact F = 0
lFact B = 1

hFact :: Horizontal -> Int
hFact L = 0
hFact R = 1

row :: String -> Int
row codes =
  sum $ zipWith (*) (reverse $ map (lFact . read . (:"")) codes) [2^n | n <- [0..]]

column :: String -> Int
column codes =
  sum $ zipWith (*) (reverse $ map (hFact . read . (:"")) codes) [2^n | n <- [0..]]

seatId :: String -> Int
seatId code =
  let
    rowCode = take 7 code
    columnCode = drop 7 code
  in
  row rowCode * 8 + column columnCode

main = do
  boardPasses <- lines <$> readFile "2020/05/input.txt"
  let ids = sort $ map seatId boardPasses
  print $ filter (\(a, b) -> b == a + 2) $ zip ids (drop 1 ids)
  print "Done!"
