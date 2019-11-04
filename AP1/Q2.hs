btoi :: String -> Int
btoi [] = 0
btoi ('0':[]) = 0
btoi ('1':[]) = 1
btoi (a:as) 
    | a == '0' = 0 + btoi as
    | otherwise = 2^((length (a:as)) - 1) + btoi as