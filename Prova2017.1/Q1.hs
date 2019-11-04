isSorted :: Ord t => [t] -> Bool
isSorted [] = False
isSorted (a:b:[]) 
    | a <= b = True
    | otherwise = False
isSorted (a:b:c:[])
    | a <= b && b <= c = True
    | otherwise = False
isSorted (a:b:abs) 
    | a <= b = isSorted abs
    | otherwise = False
isSorted [a] = True 