applyTwice :: (t -> t) -> t -> t
applyTwice f x = f (f x)

applyNtimes :: Int -> (t -> t) -> t -> t
applyNtimes 0 f x = x
--applyNtimes 1 f x =  f x
applyNtimes n f x = applyNtimes (n - 1) f (f x)

suma1 :: Int -> Int
suma1 x = x + 1

total :: (Int -> Int) -> Int -> Int 
total f 0 = f 0
total f n = total f (n - 1) + f n 

sq :: Int -> Int
sq x = x * x

sumSquares :: Int -> Int 
sumSquares n = total sq n


maxi :: Int -> Int -> Int
maxi a b 
    | a >= b = a
    | otherwise = b

maxFun :: (Int -> Int) -> Int -> Int 
maxFun f 0 = f 0
maxFun f n = maxi (maxFun f (n - 1)) (f n)

zeroInRange :: (Int -> Int) -> Int -> Bool 
zeroInRange f 0 = (f 0 == 0)
zeroInRange f n = zeroInRange f (n-1) || (f n == 0)

isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f 0 = False 
isCrescent f 1 = f 1 >= f 0
isCrescent f n = f n >= f (n-1) && isCrescent f (n-1)

vendas :: Int -> Int
vendas 0 = 2
vendas 1 = 2
vendas 2 = 3
vendas 3 = 1
vendas 4 = 1 
vendas 5 = 3 
vendas n = 0

crescente :: Int -> Int
crescente n = n

double :: [Int] -> [Int]
double [] = []
double (a:as) = (2*a) : double as

sqrList :: [Int] -> [Int]
sqrList [] = []
sqrList (a:as) = (a*a) : sqrList as

times2 :: Int -> Int
times2 n = 2 * n

mapX :: (t -> u) -> [t] -> [u]
mapX f [] = []
mapX f (a:as) = f a : mapX f as

doubleList :: [Int] -> [Int]
doubleList xs = mapX times2 xs

sqreList :: [Int] -> [Int]
sqreList xs = mapX sq xs

snds :: [(t, u)] -> [u]
snds xs = mapX snd xs

sumList :: [Int] -> Int
sumList [] = 0
sumList (a:as) = a + sumList as 

foldr1 :: (t -> t -> t) -> [t] -> t
foldr1 f [a] = a
foldr1 f (a:as) = f a (foldr1 f as)
