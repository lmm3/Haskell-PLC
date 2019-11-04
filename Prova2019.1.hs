--Questão 1
meio :: String -> Int -> Int -> String 
meio [] _ _ = []
meio _ _ 0 = []
meio (str:strs) a b 
    | b < 0 || a > length (str:strs) = []
    | a /= 1 = meio strs (a - 1) b 
    | otherwise = take b (str:strs)

--Questão 2
localizar :: String -> String -> Int
localizar _ [] = 0
localizar a b 
    | length a > length b = 0
    | 