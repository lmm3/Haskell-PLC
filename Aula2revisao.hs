--Listas => Coleções de objetos do mesmo tipo
double :: [Int] -> [Int]
double [] = []
double (a:as) = a * 2 : double as

member :: [Int] -> Int -> Bool
member [] _ = False
member (a:as) b = (a == b) || member as b 

digits :: String -> String
digits "" = ""
digits (x:xs)
    | '0' <= x && '9' >= x = x : digits xs 
    | otherwise = digits xs

sumPairs :: [(Int, Int)] -> [Int]
sumPairs [] = []
sumPairs (a:as) = fst a + snd a : sumPairs as

lastN :: [Int] -> Int
lastN [] = 0
lastN (a:as)
    | as == [] = a
    | otherwise = lastN as

takeX :: Int -> [Int] -> [Int]
takeX _ [] = []
takeX 0 _ = []
takeX x (a:as) = a : takeX (x-1) as

dropX :: Int -> [Int] -> [Int]
dropX _ [] = []
dropX 0 n = n
dropX x (a:as) = dropX (x-1) as