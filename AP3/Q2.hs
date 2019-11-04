data Tree t 
    = Nilt 
    | Node t (Tree t) (Tree t)
    deriving (Show, Eq)

isBST :: Tree t -> Bool 
isBST Nilt = False 
isBST (Node n LeftT RightT) 
    | 