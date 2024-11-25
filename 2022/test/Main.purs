module Test.Main where

import Prelude

import Test.Day01 as Day01
import Test.Day02 as Day02
import Test.Day03 as Day03
import Test.Day04 as Day04
import Test.Day05 as Day05

import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  log "Let's test"
  Day01.part1
  Day01.part2
  Day02.part1
  Day02.part2
  Day03.scoreCharacter
  Day03.part1
  Day03.group
  Day03.part2
  Day04.part1
  Day04.part2
  Day05.suite
