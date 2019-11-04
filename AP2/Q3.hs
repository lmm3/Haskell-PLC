changeElementInList :: (Double -> Double -> Double) -> Int  -> Double -> [Double] -> [Double]
changeElementInList _ _ _ [] = []
changeElementInList f i v dl = take i dl ++ (f (dl!!i) v : drop (i + 1) dl)

creditAccount :: Double -> Double -> Double
creditAccount x y = x + y

debitAccount :: Double -> Double -> Double
debitAccount x y 
    | y > x = x 
    | otherwise = x - y

processBankOperations :: [Double] -> [(Int, Int, Int, Double)] -> [Double]
processBankOperations [] _ = []
processBankOperations a [] = a
processBankOperations acc ((op, rs, rd, v):oprsrdvs)
    | op == 0 = processBankOperations (changeElementInList creditAccount rs v acc) oprsrdvs 
    | op == 1 = processBankOperations (changeElementInList debitAccount rs v acc) oprsrdvs
    | op == 2 && acc!!rs >= v = processBankOperations (changeElementInList creditAccount rd v (changeElementInList debitAccount rs v acc)) oprsrdvs
    | otherwise = acc 

