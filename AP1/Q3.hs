metade :: [Int] -> ([Int], [Int])
metade [] = ([],[])
metade n = (take (quot (length n) 2) n , drop (quot (length n) 2) n)
