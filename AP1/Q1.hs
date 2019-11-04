myReverse :: String -> String
myReverse [] = []
myReverse (a:as) = myReverse as ++ (a:[])

isPalindromo :: String -> Bool 
isPalindromo [] = True
isPalindromo n = n == myReverse n 