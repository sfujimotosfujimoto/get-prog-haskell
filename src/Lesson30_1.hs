askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >>
            getLine >>=
            (\name ->
               return (nameStatement name)) >>=
            putStrLn

helloerson :: String -> String
helloerson name = "Hello, " ++ name ++ "!"

main :: IO ()
main = do
  name <- getLine
  let statement = helloerson name
  putStrLn statement
