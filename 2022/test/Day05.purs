module Test.Day05 where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.MyStack as Stack
import Data.Tuple (Tuple(..))
import Day05 (Crate(..))
import Day05 as Day05
import Effect (Effect)
import Parsing (runParser)
import Test.Assert (assertEqual)

input :: String
input = """    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"""

suite :: Effect Unit
suite = do
  initShip
  part1
  part2

initShip :: Effect Unit
initShip =
  assertEqual 
    { actual: runParser input Day05.initShip
    , expected: Right (Map.fromFoldable
                      [ Tuple 1 (Stack.fromFoldable [ Crate 'N', Crate 'Z' ])
                      , Tuple 2 (Stack.fromFoldable [ Crate 'D', Crate 'C', Crate 'M' ])
                      , Tuple 3 (Stack.fromFoldable [ Crate 'P' ])
                      ])
    }

part1 :: Effect Unit
part1 =
  assertEqual 
    { actual: Day05.part1 input
    , expected: Right 1
    }

part2 :: Effect Unit
part2 =
  assertEqual 
    { actual: Day05.part2 input
    , expected: Right 0
    }
