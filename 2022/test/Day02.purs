module Test.Day02 where

import Prelude

import Data.Either (Either(..))
import Day02 as Day02
import Effect (Effect)
import Test.Assert (assertEqual)

input :: String
input = """
A Y
B X
C Z
"""

part1 :: Effect Unit
part1 =
  assertEqual 
    { actual: Day02.part1 input
    , expected: Right 15
    }

part2 :: Effect Unit
part2 =
  assertEqual 
    { actual: Day02.part2 input
    , expected: Right 12
    }
