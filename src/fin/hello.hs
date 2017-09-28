import Data.Char


-- main :: IO ()
-- main = do
--   putStrLn "Hello, what's your name?"
--   firstName <- getLine
--   putStrLn "What's your last name?"
--   lastName <- getLine
--   let bigFirstName = map toUpper firstName 
--       bigLastName = map toUpper lastName
--   putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

main :: IO () 
main = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words 

