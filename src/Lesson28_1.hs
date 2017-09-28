doorPrize :: [Int]
doorPrize = [1000,2000,3000]

boxPrize :: [Int]
boxPrize = [500,20000]

totalPrize :: [] Int
totalPrize = (+) <$> doorPrize <*> boxPrize

boxPrize' = [(10*), (50*)]

totalPrize' :: [Int]
totalPrize' = boxPrize' <*> doorPrize

boxMultiplier = [10,50]

newOutcomes = pure (*) <*> doorPrize <*> boxMultiplier

primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
  where twoThroughN = [2 .. n]
        composite = pure (*) <*> twoThroughN <*> twoThroughN
        isNotComposite = not . (`elem` composite)

-- 28.3.4

data User = User { name :: String
                 , gamerId :: Int
                 , score :: Int
                 } deriving Show

testNames :: [String]
testNames = ["John Smith"
            , "Robert ??"
            , "Christina NULL"
            , "Randall Munroe"
            , "SFF"]

testIds :: [Int]
testIds = [ 1337
          , 0123
          , 999999
          ]

testScores :: [Int]
testScores = [0
             ,100000
             ,-99999]

testData :: [User]
testData = pure User <*> testNames <*> testIds <*> testScores


