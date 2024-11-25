module Day05 where

import Prelude

import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.Foldable (foldr)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Int as Int
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.MyStack (Stack)
import Data.MyStack as Stack
import Data.String (codePointFromChar, fromCodePointArray)
import Parsing (ParseError, Parser, fail, runParser)
import Parsing.Combinators (sepBy, (<|>), many1)
import Parsing.String (char, string)
import Parsing.String.Basic (upper, digit)

part1 :: String -> Either ParseError Int
part1 input =
  runParser input (fail "unimplemented")


data Crate = Crate Char

derive instance eqCrate :: Eq Crate
instance showCrate :: Show Crate where
  show (Crate c) = "Crate " <> show c

crate :: Parser String Crate
crate = do
  _ <- char '['
  c <- upper
  _ <- char ']'
  pure (Crate c)

type CrateStack = Stack Crate
type Ship = Map Int CrateStack

crateRow :: Parser String (List (CrateStack -> CrateStack))
crateRow =
  (initCrate <|> initSpace) `sepBy` char ' '

initCrate :: Parser String (CrateStack -> CrateStack)
initCrate = do
  c <- crate
  pure (\s -> Stack.push c s)

initSpace :: Parser String (CrateStack -> CrateStack)
initSpace = do
  _ <- string "   "
  pure identity

initShip :: Parser String Ship
initShip = do
  rows <- crateRow `sepBy` char '\n'
  indexes <- index `sepBy` char ' '
  let s = foldr (\i map -> Map.insert i Stack.empty map) Map.empty indexes
  pure (foldr (\crates map ->
    foldrWithIndex injectCrateByIndex map crates
    ) s rows)

injectCrateByIndex :: Int -> (CrateStack -> CrateStack) -> Ship -> Ship
injectCrateByIndex i initCrateFn ship =
  Map.update (Just <<< initCrateFn) (i + 1) ship

int = do
  digits <- many1 digit
  let sd = (digits
      # fromFoldable
      # map codePointFromChar
      # fromCodePointArray
      )
  case Int.fromString sd of
      Nothing -> fail ("Could not parse index " <> sd)
      Just i -> pure i

index :: Parser String Int
index = do
  _ <- char ' '
  i <- int
  _ <- char ' '
  pure i


type Step = Ship -> Ship

step :: Parser String Step
step = do
  _ <- string "move "
  count <- int
  pure identity

-- PART 2

part2 :: String -> Either ParseError Int
part2 _ =
  pure 0

