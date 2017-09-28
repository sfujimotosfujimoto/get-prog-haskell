{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text          as T
import qualified Data.Text.IO       as TI
import           System.Environment
import           System.IO



getCounts :: String -> (Int,Int,Int)
getCounts input = (charCount, wordCount,lineCount)
  where charCount = length input
        wordCount = (length . words) input
        lineCount = (length . lines) input

countsText :: (Int,Int,Int) -> String
countsText (cc,ws,lc) = unwords ["chars: "
                                , show cc
                                , " words: "
                                , show ws
                                , " lines: "
                                , show lc]


readFile :: FilePath -> IO String
readFile name = do
  inputFile <- openFile name ReadMode
  hGetContents inputFile



main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  file <- openFile fileName ReadMode
  input <- hGetContents file
  hClose file
  let summary = (countsText . getCounts) input
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
  putStrLn summary
