module Day03 where

import Prelude

import Data.Array (length)
import Data.Array as Array
import Data.Char as Char
import Data.CodePoint.Unicode (isLower)
import Data.Foldable (foldl, sum)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (split, codePointFromChar, trim)
import Data.String.CodeUnits (toCharArray)
import Data.String.Pattern (Pattern(..))


part1 :: String -> Int
part1 input =
  input 
    # split (Pattern "\n")
    # map scoreBag
    # Array.catMaybes
    # sum


scoreBag :: String -> Maybe Int
scoreBag input =
  input
    # trim
    # toCharArray
    # (\a -> Array.splitAt (Array.length a / 2) a)
    # (\{ before, after } -> { compartmentOne: Set.fromFoldable before, compartmentTwo: Set.fromFoldable after })
    # (\bag -> Set.intersection bag.compartmentOne bag.compartmentTwo)
    # Set.toUnfoldable
    # Array.head
    # map scoreCharacter


scoreCharacter :: Char -> Int
scoreCharacter c =
  if isLower (codePointFromChar c) then
    Char.toCharCode c - 96
  else
    Char.toCharCode c - 38


part2 :: String -> Int
part2 input =
  input
    # group
    # map determineBadge
    # Array.catMaybes
    # map scoreCharacter
    # sum

group :: String -> Array (Array (Set Char))
group input =
  input 
    # trim
    # split (Pattern "\n")
    # chunks 3
    # map (map (\s -> Set.fromFoldable (toCharArray s)))

determineBadge :: Array (Set Char) -> Maybe Char
determineBadge bags =
  bags
    # Array.head
    # map (\first -> foldl Set.intersection first bags)
    # map Set.toUnfoldable 
    >>= Array.head


chunks :: forall a. Int -> Array a -> Array (Array a)
chunks n as =
  if length as <= n then
    Array.singleton as
  else
  Array.cons 
    (Array.take n as)
    (chunks n (Array.drop n as))
