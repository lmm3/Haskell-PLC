type Comando = String
type Valor = Int

findWrongDiv :: [(Comando, Valor)] -> Bool
findWrongDiv [] = False
findWrongDiv ((s, v):svs) 
    | s == "Divide" && v == 0 = True
    | otherwise = False || findWrongDiv svs

operate :: [(Comando, Valor)] -> Int -> Int
operate [] x = x
operate ((s, v): svs) x 
    | s == "Multiplica" = operate (svs) (x * v)
    | s == "Soma" = operate (svs) (x + v)
    | s == "Subtrai" = operate (svs) (x - v)
    | s == "Divide" = operate (svs) (quot x v)
    | otherwise = operate (svs) x

executa :: [(Comando, Valor)] -> Int
executa [] = 0
executa a
    | findWrongDiv a == True = -666
    | otherwise = operate a 0