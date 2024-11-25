module Day01 where

import Prelude

import Data.Array (reverse, sort, take)
import Data.Foldable (maximum, sum)
import Data.Int as Int
import Data.Maybe (Maybe, fromMaybe)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.String.Utils as String


part1 :: String -> Maybe Int
part1 input =
  split (Pattern "\n\n") input
    # map countCalories
    # maximum


countCalories :: String -> Int
countCalories multipleCalories =
  multipleCalories
    # String.lines
    # map (fromMaybe 0 <<< Int.fromString)
    # sum


part2 :: String -> Int
part2 input =
  split (Pattern "\n\n") input
    # map countCalories
    # sort
    # reverse
    # take 3
    # sum


