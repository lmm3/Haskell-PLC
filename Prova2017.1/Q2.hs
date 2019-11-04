isSorted :: Ord t => [t] -> Bool
isSorted [] = False
isSorted (a:b:[]) 
    | a <= b = True
    | otherwise = False
isSorted (a:b:c:[])
    | a <= b && b <= c = True
    | otherwise = False
isSorted (a:b:abs) 
    | a <= b = isSorted (b:abs)
    | otherwise = False
isSorted [a] = True 

processa :: Ord t => t -> [t] -> [t]
processa [] = []
processa a [] = [a]
processa a (b:bs) 
    | a <= b = a : (processa b bs)
    | otherwise = b : (processa a bs)

bSort :: Ord t => [t] -> [t]
bSort [] = []
bSort (a:as) 
    | isSorted (a:as) = (a:as)
    | otherwise = bSort(processa a as)

