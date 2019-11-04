data Tree t 
    = Node t (Tree t) (Tree t)
    | Leaf t

testeOrdenado :: Tree Int 
testeOrdenado = Node 10 (Node 5 (Leaf 3) (Leaf 6)) (Node 15 (Leaf 14) (Leaf 17))
testeNaoOrdenado :: Tree Int
testeNaoOrdenado = Node 10 (Node 5 (Leaf 3) (Leaf 6)) (Node 15 (Leaf 16) (Leaf 17))

nodeValue :: Tree t -> t
nodeValue (Leaf x) = x
nodeValue (Node x leftTree rightTree) = x

isSortedTree :: Ord t => Tree t -> Bool 
isSortedTree (Leaf x) = True
isSortedTree (Node v leftTree rightTree) = (nodeValue leftTree) < v && (nodeValue rightTree) >= v && isSortedTree (leftTree) && isSortedTree (rightTree)
