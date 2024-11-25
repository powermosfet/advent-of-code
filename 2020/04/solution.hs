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

type Passport = Map.Map String String

passportList :: GenParser Char st [Passport]
passportList =
  sepBy passport emptyLine

emptyLine =
  eol >> eol

eol =
  char '\n' >> return ()

passport =
  Map.fromList <$> sepBy field fieldSeparator

fieldSeparator
  = (char ' ' >> return ())
  <|> (try $ do
    eol
    notFollowedBy eol
    )

field = do
  prop <- fieldName
  _ <- char ':'
  val <- fieldValue
  return (prop, val)

fieldName
  =   try (string "byr")
  <|> try (string "iyr")
  <|> try (string "eyr")
  <|> try (string "hgt")
  <|> try (string "hcl")
  <|> try (string "ecl")
  <|> try (string "pid")
  <|> try (string "cid")

fieldValue =
  many (noneOf ": \n")

validate passport =
  maybe False (all id) $
    sequence $
      [  validateByr <$> Map.lookup "byr" passport
      ,  validateIyr <$> Map.lookup "iyr" passport
      ,  validateEyr <$> Map.lookup "eyr" passport
      ,  validateHgt <$> Map.lookup "hgt" passport
      ,  validateHcl <$> Map.lookup "hcl" passport
      ,  validateEcl <$> Map.lookup "ecl" passport
      ,  validatePid <$> Map.lookup "pid" passport
      ]

validateByr :: String -> Bool
validateByr byr =
  let
    numeric = readMaybe byr
    valid = \n -> n >= 1920 && n <= 2002
  in
  maybe False valid numeric

validateIyr :: String -> Bool
validateIyr byr =
  let
    numeric = readMaybe byr
    valid = \n -> n >= 2010 && n <= 2020
  in
  maybe False valid numeric

validateEyr :: String -> Bool
validateEyr byr =
  let
    numeric = readMaybe byr
    valid = \n -> n >= 2020 && n <= 2030
  in
  maybe False valid numeric

validateHgt :: String -> Bool
validateHgt hgt =
  validateCm hgt || validateIn hgt

validateCm :: String -> Bool
validateCm hgt =
  let
    unit = take 2 $ reverse hgt
    numberPart = reverse $ drop 2 $ reverse hgt
    numeric = readMaybe numberPart
    valid = \n -> n >= 150 && n <= 193
  in
  unit == "mc" && maybe False valid numeric

validateIn :: String -> Bool
validateIn hgt =
  let
    unit = take 2 $ reverse hgt
    numberPart = reverse $ drop 2 $ reverse hgt
    numeric = readMaybe numberPart
    valid = \n -> n >= 59 && n <= 76
  in
  unit == "ni" && maybe False valid numeric

validateHcl :: String -> Bool
validateHcl hcl =
  let
    hash = head hcl
    numberPart = drop 1 hcl 
    valid = \c -> c `GHC.List.elem` "1234567890abcdefABCDEF"
  in
  hash == '#' && all valid numberPart && length hcl == 7

validateEcl :: String -> Bool
validateEcl ecl =
  ecl `GHC.List.elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validatePid :: String -> Bool
validatePid pid =
  let
    l = length pid
    numeric = readMaybe pid :: Maybe Int
  in
  l == 9 && isJust numeric

main = do
  result <- parseFromFile passportList "2020/04/input.txt"
  case result of
    Right passports -> 
      print $ length $ filter validate $ passports
    Left error -> do
      print "Parse error"
      print error
  print "Done!"
