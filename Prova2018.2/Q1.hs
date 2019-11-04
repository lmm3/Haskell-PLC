encode_rle :: String -> String 
encode_rle [] = []
encode_rle a = countChars a 1 

countChars :: String -> Int -> String
countChars [a] n = (show n) ++ [a]
countChars (a:b:abs) n 
    | a == b = countChars (b:abs) (n + 1)
    | otherwise = (show n) ++ [a] ++ (countChars (b:abs) 1)