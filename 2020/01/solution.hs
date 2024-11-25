#!/bin/env stack

main = do
  contents <- readFile "2020/01/input.txt"
  let numbers = read <$> lines contents
  putStrLn "Pairs"
  mapM (\(a, b) -> if a + b == 2020 then print (a, b, a*b) else return ()) [(a, b) | a <- numbers, b <- numbers]
  putStrLn ""
  putStrLn "Pairs"
  mapM (\(a, b, c) -> if a + b + c == 2020 then print (a, b, c, a*b*c) else return ()) [(a, b, c) | a <- numbers, b <- numbers, c <- numbers]
  putStrLn ""
  return ()
