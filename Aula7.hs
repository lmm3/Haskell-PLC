--Laziness 
f :: Int -> Int -> Int 
f a b = a + b 

f1 :: Int -> Int -> Int
f1 a b = a + 12

g :: Int -> Int 
g c = c + g c

f2 :: [Int] -> [Int] -> Int 
f2 (a:as) (b:bs) = a + b

filterMul :: Int -> [Int] -> [Int]
filterMul _ [] = []
filterMul a (b:bs) 
    | rem b a == 0 = filterMul a bs
    | otherwise = b : filterMul a bs

sieve ::  [Int] -> [Int]
sieve [] = []
sieve (a:as) = a : sieve (filterMul a as)