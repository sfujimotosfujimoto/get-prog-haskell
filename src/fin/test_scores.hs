module TestScores where

import           Data.List
import qualified Data.Map       as Map
import           Data.Maybe
import           Data.Semigroup

test1 :: [(Int, Int)]
test1 = [ (1, 94), (2, 87), (3, 76), (5, 45)
        , (6, 89), (7, 59), (8, 82), (9, 99)]

test2 :: [(Int, Int)]
test2 = [ (11, 76), (13, 76), (15, 81), (16, 91)
        , (17, 83), (18, 71), (20, 37), (22, 64)]

-- Test Data
data TD a = TD [Int] [Maybe a]

-- create Test Data

-- map Test Data to Map
mapTD :: Ord k => [(k, a)] -> Map.Map k a
mapTD tests = Map.fromList tests

createTD :: [Int] -> [a] -> TD a
createTD nums values = TD completeNums extendedValues
  where completeNums = [minimum nums .. maximum nums]
        theMap = mapTD (zip nums values)
        extendedValues = map (\v -> Map.lookup v theMap) completeNums


--
showPair :: Show a => Int -> (Maybe a) -> String
showPair num (Just value) = mconcat [show num, "| ", show value, "\n"]
showPair num Nothing      = mconcat [show num, "| NA\n"]

instance Show a => Show (TD a) where
  show (TD nums values) = mconcat rows
    where rows = zipWith showPair nums values


fileToTD :: [(Int, a)] -> TD a
fileToTD pairs = createTD nums values
  where (nums, values) = unzip pairs


td1 :: TD Int
td1 = fileToTD test1

td2 :: TD Int
td2 = fileToTD test2

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing)        = myMap
insertMaybePair myMap (key, (Just value)) = Map.insert key value myMap

combineTD :: TD a -> TD a -> TD a
combineTD (TD [] []) td2 = td2
combineTD td1 (TD [] []) = td1
combineTD (TD t1 v1) (TD t2 v2) = TD completeNums combinedValues
  where bothNums = mconcat [t1, t2]
        completeNums = [minimum bothNums .. maximum bothNums]
        theMap = foldl insertMaybePair Map.empty (zip t1 v1)
        updatedMap = foldl insertMaybePair theMap (zip t2 v2)
        combinedValues = map (\v -> Map.lookup v updatedMap) completeNums

instance Semigroup (TD a ) where
  (<>) = combineTD

instance Monoid (TD a) where
  mempty = TD [] []
  mappend = (<>)

mean :: (Real a) => [a] -> Double
mean xs = total/count where
  total = (realToFrac . sum) xs
  count = (realToFrac . length) xs

meanTD :: (Real a ) => TD a -> Maybe Double
meanTD (TD _ []) = Nothing
meanTD (TD nums values) = if all (== Nothing) values
                             then Nothing
                             else Just avg where
                               justVals = filter isJust values
                               cleanVals = map fromJust justVals
                               avg = mean cleanVals
