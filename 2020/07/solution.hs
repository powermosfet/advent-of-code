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
import Data.Maybe

data Qualifier 
  = Muted
  | Plaid
  | Wavy
  | Pale
  | Faded
  | Vibrant
  | Light
  | Mirrored
  | Shiny
  | Drab
  | Dark
  | Striped
  | Dotted
  | Dull
  | Posh
  | Bright
  | Clear
  | Dim
  deriving (Show, Eq)

data Color
  = Lavender
  | Aqua
  | Lime
  | Coral
  | Chartreuse
  | Indigo
  | Salmon
  | Yellow
  | Plum
  | Brown
  | Violet
  | Olive
  | Teal
  | White
  | Gold
  | Gray
  | Blue
  | Beige
  | Tomato
  | Orange
  | Maroon
  | Red
  | Crimson
  | Fuchsia
  | Silver
  | Turquoise
  | Tan
  | Magenta
  | Black
  | Cyan
  | Purple
  | Bronze
  | Green
  deriving (Show, Eq)

data Bag = Bag Qualifier Color
  deriving (Show, Eq)

type Rule = (Bag, [(Int, Bag)])

ruleSet :: GenParser Char st [Rule]
ruleSet = endBy (rule) eol

eol =
  (char '\n' >> return ()) 

rule = do
  ruleBag <- bag
  string " contain "
  subBags <- containedBags
  string "."
  return $ (ruleBag, subBags)

containedBags
  = try (string "no other bags" >> return [])
  <|> sepBy countBag (string ", ")

bag = do
  q <- qualifier
  string " "
  c <- color
  string " "
  try (string "bags") <|> try (string "bag")
  return $ Bag q c

countBag = do
  c <- int
  string " "
  b <- bag
  return (c, b)

int = do
  digits <- many digit
  return $ read digits

qualifier
    = try (string "muted"    >> return Muted)
  <|> try (string "plaid"    >> return Plaid)
  <|> try (string "wavy"     >> return Wavy)
  <|> try (string "pale"     >> return Pale)
  <|> try (string "faded"    >> return Faded)
  <|> try (string "vibrant"  >> return Vibrant)
  <|> try (string "light"    >> return Light)
  <|> try (string "mirrored" >> return Mirrored)
  <|> try (string "shiny"    >> return Shiny)
  <|> try (string "drab"     >> return Drab)
  <|> try (string "dark"     >> return Dark)
  <|> try (string "striped"  >> return Striped)
  <|> try (string "dotted"   >> return Dotted)
  <|> try (string "dull"     >> return Dull)
  <|> try (string "posh"     >> return Posh)
  <|> try (string "bright"   >> return Bright)
  <|> try (string "clear"    >> return Clear)
  <|> try (string "dim"      >> return Dim)

color
    = try (string "lavender"   >> return Lavender)
  <|> try (string "aqua"       >> return Aqua)
  <|> try (string "lime"       >> return Lime)
  <|> try (string "coral"      >> return Coral)
  <|> try (string "chartreuse" >> return Chartreuse)
  <|> try (string "indigo"     >> return Indigo)
  <|> try (string "salmon"     >> return Salmon)
  <|> try (string "yellow"     >> return Yellow)
  <|> try (string "plum"       >> return Plum)
  <|> try (string "brown"      >> return Brown)
  <|> try (string "violet"     >> return Violet)
  <|> try (string "olive"      >> return Olive)
  <|> try (string "teal"       >> return Teal)
  <|> try (string "white"      >> return White)
  <|> try (string "gold"       >> return Gold)
  <|> try (string "gray"       >> return Gray)
  <|> try (string "blue"       >> return Blue)
  <|> try (string "beige"      >> return Beige)
  <|> try (string "tomato"     >> return Tomato)
  <|> try (string "orange"     >> return Orange)
  <|> try (string "maroon"     >> return Maroon)
  <|> try (string "red"        >> return Red)
  <|> try (string "crimson"    >> return Crimson)
  <|> try (string "fuchsia"    >> return Fuchsia)
  <|> try (string "silver"     >> return Silver)
  <|> try (string "turquoise"  >> return Turquoise)
  <|> try (string "tan"        >> return Tan)
  <|> try (string "magenta"    >> return Magenta)
  <|> try (string "black"      >> return Black)
  <|> try (string "cyan"       >> return Cyan)
  <|> try (string "purple"     >> return Purple)
  <|> try (string "bronze"     >> return Bronze)
  <|> try (string "green"      >> return Green)

contains :: Bag -> [Rule] -> Bag -> Bool
contains lookForBag inTheseRules parentBag =
  let
    isThisBag = lookForBag == parentBag
    childBags = maybe [] (fmap snd) $ lookup parentBag inTheseRules
    directChild = any (== lookForBag) childBags
  in
  directChild || any (contains lookForBag inTheseRules) childBags

populate :: [Rule] -> Bag -> [Bag]
populate rulz beg =
  let
    children = maybe [] (concat . fmap (\(count, b) -> replicate count b)) $ lookup beg rulz :: [Bag]
    descendants = concat $ map (populate rulz) children
  in
  beg:descendants

main = do
  result <- parseFromFile ruleSet "2020/07/input.txt"
  case result of
    Right rulz -> do
      print $ length $ populate rulz (Bag Shiny Gold)
    Left error -> do
      print "Parse error"
      print error
  print "Done!"
