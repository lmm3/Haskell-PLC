logCartao :: String
logCartao = "14 JAN;Amazon;40.32;15 JAN;Uber;14.84;25 JAN;Uber;34.24;02 FEV;Spotify;8.50;06 FEV;Uber;6.94;05 MAR;Burger;29.90;10 MAR;Burger;24.99;15 MAR;UCI;19.00;08 ABR;Itunes;3.50;13 ABR;Picpay;20.00;"

transformSpace :: String -> String 
transformSpace [] = []
transformSpace (x:xs) 
    | x == ';' = ' ' : transformSpace xs
    | otherwise = x : transformSpace xs

stringList :: String -> [String]
stringList [] = []
stringList x = words x

splitList :: [String] -> [(Int, String, String, Double)]
splitList [] = []
splitList (a:b:c:d:abcds) = (read a, b, c, read d) : splitList abcds

clearData :: String -> [(Int, String, String, Double)]
clearData [] = []
clearData x = (splitList . stringList . transformSpace) x

coletaMeses :: String -> [(Int, String, String, Double)] -> Double
coletaMeses _ [] = 0.0 
coletaMeses x ((a, b , c, d):abcds) 
    | b == x = d + coletaMeses x abcds
    | otherwise = coletaMeses x abcds
   
logMes :: String -> String ->Double
logMes [] _ = 0.0
logMes a b = coletaMeses a (clearData b)