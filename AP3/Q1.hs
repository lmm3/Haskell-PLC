data OPS 
    = SUM
    | SUB
    | MUL
    deriving (Show, Eq)

data IntTree 
    = Nilt Int
    | Node OPS IntTree IntTree
    deriving Show

evalTree :: IntTree -> Int
evalTree (Nilt n) = n
evalTree (Node a b c) 
    | a == SUM = (evalTree b) + (evalTree c)
    | a == SUB = (evalTree b) - (evalTree c)
    | a == MUL = (evalTree b) * (evalTree c)