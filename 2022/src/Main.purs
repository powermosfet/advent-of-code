module Main where

import Prelude

import Day01 as Day01
import Day02 as Day02
import Day03 as Day03
import Day04 as Day04

import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  log "01:"
  input01 <- readTextFile UTF8 "input/01.txt"
  log (show (Day01.part1 input01))
  log (show (Day01.part2 input01))
  log "02:"
  input02 <- readTextFile UTF8 "input/02.txt"
  log (show (Day02.part1 input02))
  log (show (Day02.part2 input02))
  log "03:"
  input03 <- readTextFile UTF8 "input/03.txt"
  log (show (Day03.part1 input03))
  log (show (Day03.part2 input03))
  input04 <- readTextFile UTF8 "input/04.txt"
  log (show (Day04.part1 input04))
  log (show (Day04.part2 input04))
