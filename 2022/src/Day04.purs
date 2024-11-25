module Day04 where

import Prelude

import Data.CodePoint.Unicode (isDecDigit)
import Data.Either (Either)
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Parsing (ParseError, Parser, fail, runParser)
import Parsing.Combinators (sepBy)
import Parsing.String (char)
import Parsing.String.Basic (takeWhile1)

part1 :: String -> Either ParseError Int
part1 input =
  runParser (trim input) elfPairs
    # map (List.filter isFullyOverlapping)
    # map (List.length)

elfPairs :: Parser String (List ElfPair)
elfPairs =
  elfPair `sepBy` (char '\n')

elfPair :: Parser String ElfPair
elfPair = do
  elfOne <- range
  _ <- char ','
  elfTwo <- range
  pure { elfOne, elfTwo }

type ElfPair =
  { elfOne :: Range
  , elfTwo :: Range
  }

range :: Parser String Range
range = do
  from <- int
  _ <- char '-'
  to <- int
  pure { from, to }

int :: Parser String Int
int = do
  digits <- takeWhile1 isDecDigit
  case Int.fromString digits of
      Just i -> pure i
      Nothing -> fail ("Could not parse number " <> digits)

type Range =
  { from :: Int
  , to :: Int
  }

isFullyOverlapping :: ElfPair -> Boolean
isFullyOverlapping { elfOne, elfTwo } =
  (elfOne.from <= elfTwo.from && elfOne.to >= elfTwo.to)
  || (elfOne.from >= elfTwo.from && elfOne.to <= elfTwo.to)

-- PART 2

part2 :: String -> Either ParseError Int
part2 input =
  runParser (trim input) elfPairs
    # map (List.filter isPartiallyOverlapping)
    # map (List.length)

isPartiallyOverlapping :: ElfPair -> Boolean
isPartiallyOverlapping { elfOne, elfTwo } =
  (elfOne.from <= elfTwo.from && elfOne.to >= elfTwo.from)
  || (elfOne.from <= elfTwo.to && elfOne.to >= elfTwo.to)
  || (isFullyOverlapping { elfOne, elfTwo })

