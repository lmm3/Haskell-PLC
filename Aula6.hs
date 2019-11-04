--Classes
{-Usadas para permitir overloading de nomes -}

--Funções Polimórficas?
--(==) :: t -> t -> Bool 
--(<) :: t -> t -> Bool
--show :: t -> String

--Funções monomorficas 
capitalize :: Char -> Char 
capitalize ch = chr (ord ch + offset)
    where offset = ord 'A' - ord 'a' 

--Funções polimorficas

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs 

--Overloading
elemBool :: Bool -> [Bool] -> Bool 
elemBool x [] = False
elemBool x (y:ys) = (x == y) || elemBool x ys

allEqual :: Eq t => t -> t -> t -> Bool 
allEqual n m p = (n == m) && (m == p)

member :: Eq t => t -> [t] -> Bool 
member a [] = False 
member a (x: xs) = (a == x) || member xs

data List t 
    = Nil 
    | Cons t (List t)
    deriving Show

data Tree t 
    = NilT
    | Node t (Tree t) (Tree t)
    deriving (Eq, Ord, Show)

class Visible t 
    where toString :: t -> String
    size :: t -> Int

instance Eq Bool where 
    True == True = True
    False == False = True 
    _ == _ = False

data List t 
    = Nil 
    | Cons t (List t )
    deriving Show

instance Eq t => Eq (List t) where  
    (==) Nil Nil = True
    (==) (Cons x xs) (Cons y ys) = (x == y) && (xs == ys)
    (==) _ _ = False 