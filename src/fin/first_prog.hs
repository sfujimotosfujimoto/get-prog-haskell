module Ch041 where

toPart :: String -> String
toPart recipient = "Dear " ++ recipient ++ ",\n"

bodyPart :: String -> String
bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ".\n"

fromPart :: [Char] -> [Char]
fromPart author = "Thanks,\n" ++ author

createEmail recipient bookTitle author = (toPart recipient) ++ (bodyPart bookTitle) ++ (fromPart author)



main :: IO ()
main = do
  print "Who is the emailfor?"
  recipient <- getLine
  print "What is the Title?"
  title <- getLine
  print "Who is the Author?"
  author <- getLine
  print (createEmail recipient title author)



