data Time 
    = Egito
    | Russia
    | Arabia
    | Uruguai 
    | Ira
    | Marrocos 
    | Portugal 
    | Espanha
    | Nenhum
    deriving (Show, Eq)

type Jogo = (Time, Int, Int, Time)

jogos1 :: [Jogo] 
jogos1 = [(Egito, 1, 3, Russia), (Arabia, 0, 3, Uruguai),
          (Egito, 0, 0, Arabia), (Russia, 0, 2, Uruguai),
          (Russia, 2, 0, Arabia), (Egito, 0, 2, Uruguai),
          (Ira, 1, 1, Marrocos), (Portugal, 2, 2, Espanha),
          (Ira, 1, 2, Portugal), (Ira, 0, 1, Espanha),
          (Marrocos, 0, 3, Portugal), (Marrocos, 1, 1,Espanha)]

--a)
gols :: Time -> [Jogo] -> Int
gols _ [] = 0
gols team ((a,b,c,d):abcds) 
    | team == a = b + (gols team abcds)
    | team == d = c + (gols team abcds)
    | otherwise = gols team abcds

--b)
saldo :: Time -> [Jogo] -> Int
saldo _ [] = 0
saldo team ((a,b,c,d):abcds) 
    | team == a = (b - c) + (saldo team abcds)
    | team == d = (c - b) + (saldo team abcds)
    | otherwise = saldo team abcds

--c)
pontos :: Time -> [Jogo] -> Int
pontos _ [] = 0
pontos team ((a,b,c,d):abcds) 
    | (team == a) && (b > c)= 3 + (pontos team abcds)
    | (team == d) && (c > b) = 3 + (pontos team abcds)
    | (team == a) && (b == c) = 1 + (pontos team abcds)
    | (team == d) && (b == c) = 1 + (pontos team abcds)
    | otherwise = pontos team abcds

--d)
type Grupo = (Char, [Time])

--e)
grupoA :: Grupo
grupoA = ('A', [Egito, Russia, Arabia, Uruguai])

grupoB :: Grupo
grupoB = ('B', [Ira, Marrocos, Portugal, Espanha])

genList :: Grupo -> [Jogo] -> (Time -> [Jogo] -> Int) -> [(Time, Int)]
genList (_, []) _ _= []
genList (c, (a:as)) [] f = (a, 0) : (genList (c,as) [] f)
genList (c, (a:as)) games f = (a, (f a games)) : (genList (c, as) games f)

genNextList :: [(Time, Int)] -> [Jogo] -> (Time -> [Jogo] -> Int) -> Int -> [(Time, Int)]
genNextList [] _ _ _ = []
genNextList ((a,b):abs) [] f v = (a, 0) : (genNextList abs [] f v)
genNextList ((a,b):abs) games f v 
    | v == b = (a, (f a games)) : (genNextList abs games f v)
    | otherwise = genNextList abs games f v

maxi :: [(Time, Int)] -> Int
maxi [] = 0
maxi ((a,b):[]) = b
maxi ((a,b):abs) = max b (maxi abs)

counti :: [(Time, Int)] -> Int -> Int
counti [] _ = 0
counti ((a,b):abs) v 
    | v == b = 1 + (counti abs v)
    | otherwise = counti abs v

find :: [(Time, Int)] -> Int -> Time
find [] _ = Nenhum
find ((a,b):abs) v
    | b == v = a
    | otherwise = find abs v

remove :: [(Time, Int)] -> Int -> [(Time, Int)]
remove [] _ = []
remove ((a,b):abs) v 
    | b == v = remove abs v
    | otherwise = (a,b) : remove abs v

getTeam :: Grupo -> [Jogo] -> (Time -> [Jogo] -> Int) -> Time
getTeam gp games f = find (genList gp games f) (maxi(genList gp games f))

getTeam2 :: Grupo -> [Jogo] -> (Time -> [Jogo] -> Int) -> Time
getTeam2 gp games f = find (remove (genList gp games f) (maxi(genList gp games f))) (maxi (remove (genList gp games f) (maxi(genList gp games f))))

classificados :: Grupo -> [Jogo] -> (Time,Time)
classificados _ [] = (Nenhum, Nenhum)
classificados gp games
    | counti (genList gp games pontos) (maxi(genList gp games pontos)) <= 1 = (getTeam gp games pontos, getTeam2 gp games pontos)
    | counti (genNextList (genList gp games pontos) games saldo (maxi(genList gp games pontos)) (maxi (genNextList (genList gp games pontos) games saldo (maxi(genList gp games pontos))) <= 1 = (ge)
    | otherwise = (Nenhum, Nenhum)