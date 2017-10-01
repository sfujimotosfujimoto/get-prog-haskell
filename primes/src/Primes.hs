module Primes where
import Data.Maybe


primes :: [Int]
primes = sieve [2 .. 10000]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where noFactors = filter (not . (== 0) . (`mod` nextPrime)) rest

isPrime :: Int -> Maybe Bool
isPrime n | n < 2 = Nothing
          | n >= length primes = Nothing
          | otherwise = Just (n `elem` primes)


prop_validPrimesOnly val = if val < 2 || val >= length primes
                              then result == Nothing
                              else isJust result
  where result = isPrime val

prop_primesArePrime val = if result == Just True
                          then length divisors == 0
                          else True
  where result = isPrime val
        divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_nonPrimesAreComposite val = if result == Just False
                                    then length divisors > 0
                                    else True
  where result = isPrime val
        divisors = filter ((== 0) . (val `mod`)) [2 .. (val -1)]


unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 [] = []
unsafePrimeFactors n [] = []
unsafePrimeFactors n (next:primes) = if n `mod` next == 0
                                        then next:unsafePrimeFactors (n `div` next) (next:primes)
                                        else unsafePrimeFactors n primes

primeFactors :: Int -> Maybe [Int]
primeFactors n | n < 2 = Nothing
               | n >= length primes = Nothing
               | otherwise = Just (unsafePrimeFactors n primesLessThanN)
  where primesLessThanN = filter (<= n) primes

prop_factorsMakeOriginal val =
  if result == Nothing
  then True
  else product (fromJust result) == val
       where result = primeFactors val

prop_allFactorsPrime val =
  if result == Nothing
  then True
  else all (== Just True) resultsPrime
       where result = primeFactors val
             resultsPrime = map isPrime (fromJust result)


