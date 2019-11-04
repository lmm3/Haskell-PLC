--Input/Output Haskell 
writefoo :: IO ()
writefoo = putStrLn "foo"

myputStrLn :: String -> IO()
myputStrLn str = do
    putStr str
    putStr "\n"

somaEs :: IO ()
somaEs = do 
    putStrLn "Entre o primeiro numero"
    xs <- getLine
    putStrLn "Entre o segundo numero"
    ys <- getLine 
    putStrLn ("o resultado e " ++ show((read xs) + (read ys)))
    
putNTimes :: Int -> String -> IO()
putNTimes 0 str = return()
putNTimes n str 
    = if n <= 1
        then putStr str
        else do 
            putStr str
            putNTimes (n - 1) str



