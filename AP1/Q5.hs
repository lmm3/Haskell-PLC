transformC :: Char -> [(Char, Char)] -> String
transformC a ((b,c):[]) = c : []
transformC a ((b,c):bcs) 
    | a == b = c : []
    | otherwise = transformC a bcs 

decEnigma :: String -> [(Char, Char)] -> String
decEnigma [] _ = []
decEnigma (a:as) ltc = transformC a ltc ++ decEnigma as ltc