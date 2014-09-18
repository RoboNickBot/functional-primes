module Primes
    ( primes 
    , primes1
    , primes2
    , primes3
    , primes4
    , divis
    , isPrime
    , largestPF
    , largestPF1
    , largestPF2
    ) where

divis :: Int -> Int -> Bool
divis a b = (==0) $ mod a b

primes :: [Int]
primes = primes4

largestPF :: [Int] -> Int -> Int
largestPF = largestPF2

isPrime :: Int -> Bool
isPrime a = testPrime a primes

-- fastest
primes4 :: [Int]
primes4 = 2:(r 3 [2]) where
    r :: Int -> [Int] -> [Int]
    r a xs = if testPrime a xs then a:(r (a+1) (xs ++ [a])) else r (a+1) xs

testPrime :: Int -> [Int] -> Bool
testPrime a xs = not . or $ fmap (divis a) (candidates a xs)

-- try applying the prime-factorization method to checking if a 
-- number is prime?
--
-- divisibility should not need to be checked for this many numbers
--
-- the lowest prime factor of any number must be <= its sqaure root.
--
-- but square roots take time to compute..
-- however, squares are fast!
--
-- when checking divisibility, check the sqaure of the divisor.
-- if it is greater than the candidate, then the candidate is prime!

candidates :: Int -> [Int] -> [Int]
candidates a = takeWhile $ (<=a) . (^2)

primes1 :: [Int]
primes1 = 2:[x | x <- [3..], (not (or (fmap (divis x) [2..(x-1)])))]

primes2 :: [Int]
primes2 = 2:(r 3 [2]) where
    r :: Int -> [Int] -> [Int]
    r a xs = if p a xs then a:(r (a+1) (xs ++ [a])) else r (a+1) xs
    p :: Int -> [Int] -> Bool
    p a xs = not (or (fmap (divis a) xs))

primes3 :: [Int]
primes3 = 2:(r 3 [2]) where
    r :: Int -> [Int] -> [Int]
    r a xs = if p a xs then a:(r (a+1) (xs ++ [a])) else r (a+1) xs
    p :: Int -> [Int] -> Bool
    p a xs = not . or $ fmap (divis a) (takeWhile (<=(quot a 2)) xs)

largestPF1 :: [Int] -> Int -> Int
largestPF1 prs a = f a (reverse $ takeWhile (<=(quot a 2)) prs) where
    f :: Int -> [Int] -> Int
    f a (p:ps) = if divis a p then p else f a ps

-- fastest
largestPF2 :: [Int] -> Int -> Int
largestPF2 (p:ps) u
    | p == u = p
    | divis u p = largestPF2 (p:ps) (quot u p)
    | otherwise = largestPF2 ps u
