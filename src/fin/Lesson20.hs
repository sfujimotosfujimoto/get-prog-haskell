module Lesson20 where

import System.Random

  
-- helloPerson :: String -> String
-- helloPerson name = "Hello " ++ " " ++ name ++ "!"

-- main :: IO ()
-- main = do
--   putStrLn "Hello! What's your name?"
--   name <- getLine
--   let statement = helloPerson name
--   putStrLn statement

-- minDie :: Int
-- minDie = 1

-- maxDie :: Int
-- maxDie = 6

-- main :: IO ()
-- main = do
--   dieRoll <- randomRIO (minDie, maxDie)
--   putStrLn (show dieRoll)

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++  "!"

main :: IO ()
main = do
  putStrLn "Hello! What's your name?"
  name <- getLine
  let statement = helloPerson name
  putStrLn statement
 
