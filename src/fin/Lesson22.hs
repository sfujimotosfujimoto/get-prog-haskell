{-# LANGUAGE OverloadedStrings #-}
module Lesson22 where 

import qualified Data.Text as T
import Data.Semigroup

import Control.Monad
import Data.Char

import Data.List.Split 



firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord 

myWord :: T.Text
myWord = "dog"

someText :: T.Text
someText = "Some\ntext for\t you"

breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"

sampleInput :: T.Text
sampleInput = "this\nis\ninput"















main :: IO ()
main = do
  colors <- forM [1,2,3,4] (\a -> do
    putStrLn $ "Which color do you associate with the number" ++ show a ++ "?"
    color <- getLine
    return color)
  putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
  mapM_ putStrLn colors



