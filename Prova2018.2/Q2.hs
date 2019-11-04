decode_rle :: String -> String
decode_rle [] = []
decode_rle (a:b:abs) = (printNTimes (charToInt a) b) ++ (decode_rle abs)

charToInt :: Char -> Int 
charToInt ch = fromEnum ch - fromEnum '0'

printNTimes :: Int -> Char -> String
printNTimes 0 _ = []
printNTimes n c = c : (printNTimes (n - 1) c)