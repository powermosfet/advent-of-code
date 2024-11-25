#!/bin/env stack
{- stack
   script
   --resolver lts-16.24
   --package containers
   --package text
   --package parsec
   --package foldl
   --package ghc
   --package ilist
 -}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import Text.ParserCombinators.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Char as Char
import qualified Control.Foldl as Fold
import qualified Data.Map as Map
import Text.Read
import GHC.List (elem, uncons)
import Data.Maybe
import Util
import Data.List.Index

data Selection a =
  Selection
    { before :: [a]
    , current :: a
    , after :: [a]
    }

forwards :: Selection a -> Selection a
forwards selection =
  Selection
    { before = current selection : before selection
    , current = head (after selection)
    , after = tail (after selection)
    }

backwards :: Selection a -> Selection a
backwards selection =
  Selection
    { before = tail (before selection)
    , current = head (before selection)
    , after = current selection : after selection
    }

updateCurrent :: (a -> a) -> Selection a -> Selection a
updateCurrent fn selection =
  Selection
    { before = before selection
    , current = fn (current selection)
    , after = after selection
    }

fromList :: [a] -> (Selection a)
fromList l =
  let
    h = head l
    t = tail l
  in
  Selection
    { before = []
    , current = h
    , after = t
    }


data Instruction
  = Nop Int
  | Jmp Int
  | Acc Int
  | Terminate
  deriving (Show, Eq)

toggle :: Instruction -> Instruction
toggle (Nop a) = Jmp a
toggle (Jmp a) = Nop a
toggle (Acc a) = Acc a

program :: GenParser Char st [Instruction]
program = endBy (instruction) eol

eol =
  (char '\n' >> return ()) 

instruction = do
  op <- operation
  string " "
  arg <- argument
  return $ op arg

operation
  =   try (string "nop" >> return Nop)
  <|> try (string "acc" >> return Acc)
  <|> try (string "jmp" >> return Jmp)

argument = do
  s <- sign
  n <- number
  return $ s * n

sign = (string "+" >> return 1) <|> (string "-" >> return (-1))

number = read <$> many digit

data Machine = Machine (Selection (Instruction, Bool)) Int

run :: Machine -> Maybe Int
run machine@(Machine instructions accumulator) =
  let
    (curr, hasRunBefore) = current instructions
  in
  if hasRunBefore then
    Nothing
  else
    if curr == Terminate then
      Just accumulator
    else
      run $ executeInstruction curr machine

executeInstruction :: Instruction -> Machine -> Machine
executeInstruction instr (Machine instructions_ accumulator) =
  let 
    instructions = updateCurrent (\(i, _) -> (i, True)) instructions_
  in
  case instr of
    Nop _ ->
      Machine (forwards instructions) accumulator

    Jmp count -> 
      if count > 0 then
        Machine ((nTimes count forwards) instructions) accumulator
      else
        Machine ((nTimes (count * (-1)) backwards) instructions) accumulator

    Acc change ->
      Machine (forwards instructions) (accumulator + change)

addFlags = map (\x -> (x, False)) 

main = do
  result <- parseFromFile program "2020/08/input.txt"
  case result of
    Right originalProgram -> do
      let terminatingProgram = originalProgram ++ [ Terminate ]
      let machines = imap (\index instr -> Machine (fromList (addFlags (setAt index (toggle instr) terminatingProgram))) 0) originalProgram
      print $ filter isJust $ (map run machines)
      return ()
    Left error -> do
      print "Parse error"
      print error
  print "Done!"
