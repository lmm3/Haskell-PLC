answer :: Int 
answer = 42

greater :: Bool 
greater = (answer > 71)

yes :: Bool 
yes = True

inc:: Int -> Int 
inc x = x + 1

square :: Int -> Int 
square x = x * x

allEqual :: Int -> Int -> Int -> Bool 
allEqual m n p = (m == n) && (n == p)

avarage:: Float -> Float -> Float
avarage a b = (a + b)/2.0

maxi:: Int -> Int -> Int 
maxi a b 
    | a >= b = a
    | otherwise = b 

addD :: Int -> Int -> Int
addD a b = 2 * (a * b)

myNot :: Bool -> Bool
myNot True = False
myNot False = True

myOr :: Bool -> Bool -> Bool 
myOr True x = True
myOr False x = x

myAnd :: Bool -> Bool -> Bool 
myAnd True x = x
myAnd False x =  False

fat :: Int -> Int 
fat n
    | n == 0 = 1 
    | n > 0 = n * fat (n - 1) 

all4Equal :: Int -> Int -> Int -> Int -> Bool 
all4Equal a b c d = allEqual a b c && allEqual b c d 

all2Equal :: Int -> Int -> Bool
all2Equal a b = a == b

equalCount :: Int -> Int -> Int -> Int
equalCount a b c
    | allEqual a b c = 3
    | all2Equal a b && all2Equal a c && all2Equal b c = 2
    | otherwise = 0

intP :: (Int, Int)
intP = (33, 43)

addPair :: (Int, Int) -> Int
addPair (x, y) = x+y

shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift ((x, y), z) = (x, (y, z))

type Name = String
type Age = String
type Phone = Int 
type Person = (Name, Age, Phone)

name :: Person -> Name
name (n, a, p) = n

addEspacos :: Int -> String 
addEspacos n 
    | n > 0 = addEspacos (n - 1) ++ " "
    | n == 0 = ""

paraDireita :: Int -> String -> String
paraDireita a b = addEspacos (a) ++ b 

vendas :: Int -> Float
vendas 0 = 12.0
vendas 1 = 14.0
vendas 2 = 15.0
vendas 3 = 20.0 
vendas 4 = 23.0 
vendas 5 = 12.0
vendas n = 0

imprimeTabela :: Int -> IO()
imprimeTabela n = putStr(cabecalho
                        ++ imprimeSemana n 
                        ++ imprimeTotal n
                        ++ imprimeMedia n 
                        )

imprimeSemana :: Int -> String
imprimeSemana n
    | n == 0 = "   " ++ show (0) ++ "    " ++ show(vendas 0) ++ "\n"
    | n > 0 = imprimeSemana(n - 1) ++ "   " ++ show (n) ++ "    " ++ show(vendas n) ++ "\n"
 
imprimeTotal :: Int -> String
imprimeTotal n = "Total   " ++ show (totalVendas n) ++ "\n"

imprimeMedia :: Int -> String 
imprimeMedia n = "Media   " ++ show (mediaVendas n) ++ "\n"

totalVendas :: Int -> Float 
totalVendas n 
    | n == 0 = vendas 0
    | n > 0 = vendas n + totalVendas (n - 1)

mediaVendas :: Int -> Float
mediaVendas n = totalVendas n / fromIntegral(n + 1)

cabecalho :: String
cabecalho = "Semanas Vendas\n"

roots :: Float -> Float -> Float -> String
roots a b c
    | b^2 > 4.0*a*c = show f ++ " " ++ show s
    | b^2 == 4.0*a*c =  show (oneRoot a b c)
    | otherwise = "no roots"
    where (f, s) = twoRoots a b c 

oneRoot :: Float -> Float -> Float -> Float
oneRoot a b c = -b/2*a

twoRoots :: Float -> Float -> Float -> (Float, Float)
twoRoots a b c = (d - e , d + e)
    where 
    d = -b/2*a
    e = sqrt(b^2-4.0*a*c)

