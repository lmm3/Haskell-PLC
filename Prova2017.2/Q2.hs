substr :: String -> String -> Bool
substr [] [] = True
substr [] _ = False
substr _ [] = False
substr str1 str2 
    | (length str1) > (length str2) = False
    | otherwise = substrAux str1 str2 (length str1)

substrAux :: String -> String -> Int -> Bool 
substrAux [] [] _ = True
substrAux [] _ _ = False
substrAux _ [] _ = False
substrAux a (b:bs) n 
    | a == take n (b:bs) = True
    | otherwise = substrAux a bs n