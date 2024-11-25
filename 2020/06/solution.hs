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
import qualified Data.Set as Set
import Text.Read
import GHC.List (elem)
import Data.Maybe

groups :: GenParser Char st [[String]]
groups =
  sepBy group emptyLine

emptyLine =
  eol >> eol

eol =
  char '\n' >> return ()

group =
  sepBy form (try $ do
    eol
    notFollowedBy eol
    )

form =
  many (oneOf "abcdefghijklmnopqrstuvwxyz")

getGroupCount forms =
  Set.size . foldl Set.intersection (Set.fromList "abcdefghijklmnopqrstuvwxyz") $ map Set.fromList forms

main = do
  result <- parseFromFile groups "2020/06/input.txt"
  case result of
    Right groups -> 
      print $ sum $ map getGroupCount groups
    Left error -> do
      print "Parse error"
      print error
  print "Done!"
