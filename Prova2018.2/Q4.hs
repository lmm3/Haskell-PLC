type DicionarioT = Tree Int String 
data Tree chave valor 
    = Node chave valor (Tree chave valor) (Tree chave valor)
    | Leaf

meuDicionario :: DicionarioT
meuDicionario = Node 4 "teria" (Node 3 "cafe" (Node 1 "casa" Leaf Leaf) Leaf) (Node 6 "era" Leaf (Node 7 "uma" Leaf Leaf))

teste = "a 1 tinha 3 mas nao 6 7 34, a 1 6 7 sorve4"

substitute :: DicionarioT -> Int -> String 
substitute Leaf _ = []
substitute (Node key value (leftTree) (rightTree)) n
    | n == key = value
    | n > key = substitute rightTree n
    | n < key = substitute leftTree n

decodeTree :: DicionarioT -> String -> String
decodeTree Leaf a = a
decodeTree _ [] = []
decodeTree arv (str:strs) 
    | str <= '9' && str >= '0' = (substitute arv (charToInt str)) ++ (decodeTree arv strs)
    | otherwise = str : (decodeTree arv strs)

charToInt :: Char -> Int 
charToInt ch = fromEnum ch - fromEnum '0'
