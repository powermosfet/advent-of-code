module Test.Day04 where

import Prelude

import Day04 as Day04

import Data.Either (Either(..))
import Effect (Effect)
import Test.Assert (assertEqual)

input :: String
input = """
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
"""

part1 :: Effect Unit
part1 =
  assertEqual 
    { actual: Day04.part1 input
    , expected: Right 2
    }

part2 :: Effect Unit
part2 =
  assertEqual 
    { actual: Day04.part2 input
    , expected: Right 4
    }
