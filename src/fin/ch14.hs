module Chapter14 where


data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

{-
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where halfAlphabet = n `div` 2
        offset = fromEnum c + halfAlphabet
        rotation = offset `mod` alphabetSize
-}

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
 where halfAlphabet = c `div` 2
       offset = fromEnum c + halfAlphabet
       rotation =  offset `mod` alphabetSize

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

{-}
rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
  where
    sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)
-}

main :: IO ()
main = undefined

