import System.IO


main :: IO ()
main = do
  userInput <- getContents
  mapM_ print userInput