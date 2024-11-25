module Day02 where

import Prelude

import Data.Either (Either)
import Data.Eq (class Eq)
import Data.Foldable (sum)
import Data.List (List)
import Data.String (trim)
import Effect.Exception (throwException)
import Parsing (ParseError, Parser, runParser)
import Parsing.Combinators (sepBy, (<|>))
import Parsing.String (char)
import Parsing.String.Basic (space)

part1 :: String -> Either ParseError Int
part1 input =
  runParser (trim input) rounds
    # map (\parsedRounds ->
        parsedRounds
          # map score
          # sum
          )


part2 :: String -> Either ParseError Int
part2 input =
  runParser (trim input) rounds'
    # map (\parsedRounds ->
        parsedRounds
          # map score
          # sum
          )


data Move
  = Rock
  | Paper
  | Scissors

derive instance eqMove :: Eq Move


type Round =
  { opponent :: Move
  , self :: Move
  }


opponentMove :: Parser String Move
opponentMove =
  opponentRock <|> opponentPaper <|> opponentScissors

opponentRock :: Parser String Move
opponentRock = do
  _ <- char 'A'
  pure Rock

opponentPaper :: Parser String Move
opponentPaper = do
  _ <- char 'B'
  pure Paper

opponentScissors :: Parser String Move
opponentScissors = do
  _ <- char 'C'
  pure Scissors

selfMove :: Parser String Move
selfMove =
  selfRock <|> selfPaper <|> selfScissors

selfRock :: Parser String Move
selfRock = do
  _ <- char 'X'
  pure Rock

selfPaper :: Parser String Move
selfPaper = do
  _ <- char 'Y'
  pure Paper

selfScissors :: Parser String Move
selfScissors = do
  _ <- char 'Z'
  pure Scissors

round :: Parser String Round
round = do
  o <- opponentMove
  _ <- space
  s <- selfMove
  pure { opponent: o, self: s }

rounds :: Parser String (List Round)
rounds =
  round `sepBy` (char '\n')

score :: Round -> Int
score r =
  let
      moveScore = case r.self of
                      Rock -> 1
                      Paper -> 2
                      Scissors -> 3

      resultScore = case result r of
                        Lose -> 0
                        Draw -> 3
                        Win -> 6
  in
  moveScore + resultScore


data Result 
  = Lose 
  | Draw
  | Win

derive instance eqResult :: Eq Result

result :: Round -> Result
result { opponent, self } =
  case opponent of
      Rock -> 
        case self of
            Rock -> Draw
            Paper -> Win
            Scissors -> Lose
      Paper -> 
        case self of
            Rock -> Lose
            Paper -> Draw
            Scissors -> Win
      Scissors -> 
        case self of
            Rock -> Win
            Paper -> Lose
            Scissors -> Draw


-- PART 2


rounds' :: Parser String (List Round)
rounds' =
  round' `sepBy` (char '\n')

round' :: Parser String Round
round' = do
  o <- opponentMove
  _ <- space
  sr <- selfResult
  let s = findSelfMove o sr
  pure { opponent: o, self: s }

findSelfMove :: Move -> Result -> Move
findSelfMove o desiredResult =
  if result { opponent: o, self: Rock } == desiredResult then
    Rock
  else if result { opponent: o, self: Paper } == desiredResult then
    Paper
  else
    Scissors

selfResult :: Parser String Result
selfResult =
  selfLose <|> selfDraw <|> selfWin

selfLose :: Parser String Result
selfLose = do
  _ <- char 'X'
  pure Lose

selfDraw :: Parser String Result
selfDraw = do
  _ <- char 'Y'
  pure Draw

selfWin :: Parser String Result
selfWin = do
  _ <- char 'Z'
  pure Win

