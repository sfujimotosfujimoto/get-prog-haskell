import qualified Data.Map as Map
import Control.Monad
import Data.Char


data Grade = F | D | C | B | A  deriving (Show, Eq, Ord, Enum, Read)

data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
  { candidateId :: Int
  , codeReview :: Grade
  , cultureFit :: Grade
  , education :: Degree } deriving Show


viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where passedCoding = codeReview candidate > B
        passedCultureFit = cultureFit candidate > C
        educationMin = education candidate >= MS
        tests = [ passedCoding
                , passedCultureFit
                , educationMin ]

candidate1 :: Candidate
candidate1 = Candidate 1 A A BA

candidate2 :: Candidate
candidate2 = Candidate 2 C A PhD

candidate3 :: Candidate
candidate3 = Candidate 3 A B MS

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [ (1, candidate1)
                           , (2, candidate2)
                           , (3, candidate3)]

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = do
  readG <- getLine
  return (read readG)
  
  -- getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readCandidate :: IO Candidate
readCandidate = do
  putStrLn "enter id: "
  cId <- readInt
  putStrLn "enter code grade: "
  codeGrade <- readGrade
  putStrLn "enter culture fit grade: "
  cultureGrade <- readGrade
  putStrLn "enter education: "
  degree <- readDegree
  return (Candidate { candidateId = cId
                    , codeReview = codeGrade
                    , cultureFit = cultureGrade
                    , education = degree })

assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
  candidate <- Map.lookup cId candidateDB
  let passed = viable candidate
  let statement = if passed
                     then "passed"
                     else "failed"
  return statement


failPassOrElse :: Maybe String -> String
failPassOrElse Nothing = "error id not found"
failPassOrElse (Just val) = val

candidates :: [Candidate]
candidates = [ candidate1
             , candidate2
             , candidate3 ]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement

assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement = if passed
                     then "passed"
                     else "failed"
  return statement


--

allSquares :: Int -> [(Int, Int)]
allSquares n = do
  nums <- [1 .. n]
  -- let idx = nums
  let squares = nums ^ n
  return (nums, squares)


evensGuard :: Int -> [Int]
evensGuard n = do
  value <- [1 .. n]
  guard(even value)
  return value

words'' :: [String]
words'' = ["brown", "blue", "pink", "orange"]

toUpper' :: [String] -> [String]
toUpper' words = [ wordC | word <- words
                         , let word' =  (\(x:xs) -> toUpper x:xs) word
                         , let wordC = "Mr. " ++ word'] 
