type Dicionario = [(Int, String)]

meuDicionario :: Dicionario 
meuDicionario = [(1, "casa"), (3, "cafe"), (4, "teria"), (6, "era"), (7, "uma")]

teste = "a 1 tinha 3 mas nao 6 7 34, a 1 6 7 sorve4"

decode :: Dicionario -> String -> String 
decode [] a = a 
decode _ [] = []
decode dic (a: as) 
    | a >= '0' && a <= '9' = (substitute dic (charToInt a)) ++ (decode dic as)
    | otherwise = a : (decode dic as)

substitute :: Dicionario -> Int -> String
substitute [] _ = []
substitute ((a,b):abs) n 
    | a == n = b
    | otherwise = substitute abs n 

charToInt :: Char -> Int 
charToInt ch = fromEnum ch - fromEnum '0'