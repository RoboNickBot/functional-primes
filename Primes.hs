module Primes
    ( primes 
    , divis
    , isPrime
    , largestPF
    ) where

divis :: Int -> Int -> Bool
divis a b = (==0) $ mod a b

isPrime :: Int -> Bool
isPrime a = testPrime a primes


primes :: [Int]
primes = 2 : (r 3 [2]) where
    -- Recursive helper function
    r :: Int -> [Int] -> [Int]
    r a ps =
        if testPrime a ps 
        -- Add 'a' to the list of primes and the checklist
        then let rest = r (a+2) (ps++[a]) in a : rest
        -- Forget 'a' and move on to the next number
        else r (a+2) ps

testPrime :: Int -> [Int] -> Bool
testPrime a ps = not . or $ fmap (divis a) (candidates a ps)

-- Values that need to be checked for divisibility
candidates :: Int -> [Int] -> [Int]
candidates a = takeWhile $ (<=a) . (^2)


largestPF :: [Int] -> Int -> Int
largestPF (p:ps) u
    | p == u = p
    | divis u p = largestPF (p:ps) (quot u p)
    | otherwise = largestPF ps u
