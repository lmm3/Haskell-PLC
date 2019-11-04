sumList :: [Int] -> Int
sumList n
    | n == [] = 0
    | otherwise = head n + sumList(tail n)

lastN :: [Int] -> Int
lastN [] = 0
lastN [a] = a
lastN (a:as) = lastN as

doubleList :: [Int] -> [Int]
doubleList n 
    | n == [] = []
    | otherwise = 2*(head n) : doubleList(tail n)

member :: [Int] -> Int -> Bool
member [] a = False
member (a:as) n = a == n || member as n

sumPairs :: [(Int, Int)] -> [Int]
sumPairs [] = []
sumPairs (a: as) = fst(a) +snd(a) : sumPairs(as)

digits :: String -> String
digits [] = []
digits (a:as) 
    | a >= '0' && a <= '9' = a : digits as
    | otherwise = digits as

takeX :: Int -> [Int] -> [Int]
takeX _ [] = []
takeX 0 _ = []
takeX n (a:as) = a : takeX (n - 1) as 

dropX :: Int -> [Int] -> [Int] 
dropX _ [] = []
dropX 0 n = n 
dropX x (a:as) = drop (x - 1) as

firstDigit :: String -> Char 
firstDigit st = case (digits st) of
                [] -> '\0'
                (a:as) -> a

type Pessoa = String
type Livro = String
type BancoDeDados = [(Pessoa, Livro)]

baseExemplo :: BancoDeDados 
baseExemplo = 
    [("Sergio", "O Senhor dos Aneis"),
    ("Andre", "Duna"),
    ("Fernando", "Jonathan Strange & Mr. Norrell"),
    ("Fernando", "Duna")]
--livros emprestados

livros :: BancoDeDados -> Pessoa -> [Livro]
livros [] _ = []
livros _ [] = []
livros (a:as) x 
    | x == fst a = snd a : livros as x
    | otherwise = livros as x

emprestimos :: BancoDeDados -> Livro -> [Pessoa]
emprestimos [] _ = []
emprestimos _ [] = []
emprestimos (a:as) x 
    | x == snd a = fst a : emprestimos as x
    | otherwise = emprestimos as x 

emprestado :: BancoDeDados -> Livro -> Bool
emprestado [] _ = False 
emprestado _ [] = False 
emprestado (a:as) x = snd a == x || emprestado as x 

qtdEmprestimos :: BancoDeDados -> Pessoa -> Int
qtdEmprestimos [] _ = 0
qtdEmprestimos _ [] = 0
qtdEmprestimos (a:as) x 
    | fst a == x = 1 + qtdEmprestimos as x
    | otherwise = qtdEmprestimos as x 

emprestar :: BancoDeDados -> Pessoa -> Livro -> BancoDeDados
emprestar a [] _ = a
emprestar a _ [] = a
emprestar a b c = (b, c) : a

devolver :: BancoDeDados -> Pessoa -> Livro -> BancoDeDados
devolver [] _ _ = []
devolver a [] _ = a
devolver a _ [] = a
devolver (a:as) b c 
    | b == fst a && c == snd a = as
    | otherwise = a : devolver as b c

