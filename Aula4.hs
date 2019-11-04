--Função de Composição
somaUm :: Int -> Int 
somaUm x = x + 1

twice :: (t -> t) -> (t -> t)
twice f = f . f

iter :: Int -> (t -> t) -> (t -> t)
iter 0 f = id 
iter n f = (iter (n - 1) f) . f 

multiply = \x -> \y -> x * y
double = multiply 2

