#!/bin/env stack
{- stack
   script
   --resolver lts-6.25
   --package turtle
   --
   +RTS -s -RTS
 -}

valid (sLowerBound : sUpperBound : c : password : _) =
  let
    lowerBound = read sLowerBound
    upperBound = read sUpperBound
    count = length $ filter (== (head c)) password
  in
  lowerBound <= count && upperBound >= count

checkC index c password =
  (password !! (index - 1)) == head c

valid2 (sLowerBound : sUpperBound : c : p : _) =
  let
    lowerBound = read sLowerBound
    upperBound = read sUpperBound
  in
  ((checkC lowerBound c p) || (checkC upperBound c p)) && (not ((checkC lowerBound c p) && (checkC upperBound c p)))

main = do
  contents <- readFile "2020/02/input_processed.txt"
  let passwords = words <$>  lines contents
  print $ length $ filter valid2 passwords
  return ()
