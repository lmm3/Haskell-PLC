isReplica :: String -> Int -> Char -> Bool
isReplica [] _ _ = False
isReplica _ 0 _ = False
isReplica (a:[]) 1 c
    | a == c = True
    | otherwise = False
isReplica (a:as) n c 
    | length (a:as) /= n = False
    | otherwise = (a == c) && isReplica as (n-1) c