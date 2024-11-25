module Test.Day03 where

import Prelude

import Day03 as Day03

import Data.Set as Set
import Effect (Effect)
import Test.Assert (assertEqual)

input :: String
input = """
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
"""

part1 :: Effect Unit
part1 =
  assertEqual 
    { actual: Day03.part1 input
    , expected: 157
    }

scoreCharacter :: Effect Unit
scoreCharacter = do
  assertEqual 
    { actual: Day03.scoreCharacter 'a'
    , expected: 1
    }
  assertEqual 
    { actual: Day03.scoreCharacter 'z'
    , expected: 26
    }
  assertEqual 
    { actual: Day03.scoreCharacter 'A'
    , expected: 27
    }
  assertEqual 
    { actual: Day03.scoreCharacter 'Z'
    , expected: 52
    }


group :: Effect Unit
group =
  assertEqual 
    { actual: Day03.group input
    , expected: [ [ (Set.fromFoldable ['F','J','M','W','c','f','g','h','p','r','s','t','v','w'])
                  , (Set.fromFoldable ['D','F','G','H','L','M','N','R','S','Z','f','j','q','r','s','z'])
                  , (Set.fromFoldable ['B','P','T','V','W','d','g','m','q','r','v','w','z'])
                  ]
                , [ (Set.fromFoldable ['B','F','H','L','M','Q','S','T','Z','b','c','h','j','n','q','v','w'])
                  , (Set.fromFoldable ['G','J','Q','R','T','Z','c','g','t'])
                  , (Set.fromFoldable ['C','D','G','J','L','M','P','Z','m','p','r','s','w','z'])
                  ]
                ]
    }

part2 :: Effect Unit
part2 =
  assertEqual 
    { actual: Day03.part2 input
    , expected: 70
    }
