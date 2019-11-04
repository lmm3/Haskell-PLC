locate :: Eq t => t -> [t] -> Int
locate _ [] = -1
locate v l 
    | find v l = locateAux v l 0
    | otherwise = -1 

locateAux :: Eq t => t -> [t] -> Int -> Int
locateAux _ [] _ = -1
locateAux v [a] _ = 0
locateAux v (a:as) n 
    | v == a = n
    | otherwise = locateAux v as (n + 1)

find :: Eq t => t -> [t] -> Bool
find _ [] = False
find v (a:as) 
    | v == a = True
    | otherwise = find v as 