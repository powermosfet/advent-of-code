module Test.Day01 where

import Prelude

import Data.Maybe (Maybe(..))
import Day01 as Day01
import Effect (Effect)
import Test.Assert (assertEqual)

input :: String
input = """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""

part1 :: Effect Unit
part1 =
  assertEqual 
    { actual: Day01.part1 input
    , expected: Just 24000
    }

part2 :: Effect Unit
part2 =
  assertEqual 
    { actual: Day01.part2 input
    , expected: 45000
    }
