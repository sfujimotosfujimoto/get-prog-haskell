import Lib
import Data.Char (isPunctuation)
import Data.Text as T
import Test.QuickCheck
import Test.QuickCheck.Instances

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
                                          then putStrLn passStatement
                                          else putStrLn failStatement

prop_punctuationInvariant text = preprocess text ==
                                 preprocess noPuncText
  where noPuncText = T.filter (not . isPunctuation) text


main :: IO ()
main = do
  quickCheckWith stdArgs { maxSuccess = 1000 }  prop_punctuationInvariant
  -- quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
  putStrLn "done!"


