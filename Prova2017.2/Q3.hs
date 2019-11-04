data Command 
    = TurnLeft
    | TurnRight
    | Forward Int
    | Backward Int
    deriving (Show)

data Direction 
    = North
    | South
    | West
    | East
    deriving (Show)

destination :: (Int, Int) -> [Command] -> (Int, Int)
destination (x, y) [] = (x, y)
destination (x, y) commands = move (0, 0) commands (North)

move :: (Int, Int) -> [Command] -> Direction -> (Int, Int)
move (x, y) [] _ = (x, y)
move (x, y) ((Forward n):cms) (direc) = (move (goForward (x, y) direc n) cms direc)
move (x ,y) ((Backward n):cms) (direc) = (move (goBackward (x, y) direc n) cms direc)
move (x, y) ((TurnLeft):cms) (direc) = (move (x, y) cms (goLeft direc)) 
move (x, y) ((TurnRight):cms) (direc) = (move (x, y) cms (goRight direc))

goForward :: (Int, Int) -> Direction -> Int -> (Int, Int)
goForward (x, y) (North) n = (x, y + n)
goForward (x, y) (South) n = (x, y - n)
goForward (x, y) (East) n = (x + n, y)
goForward (x, y) (West) n = (x - n, y)

goBackward :: (Int, Int) -> Direction -> Int -> (Int, Int)
goBackward (x, y) (North) n = (x, y - n)
goBackward (x, y) (South) n = (x, y + n)
goBackward (x, y) (East) n = (x - n, y)
goBackward (x, y) (West) n = (x + n, y)

goLeft :: Direction -> Direction
goLeft (North) = (West)
goLeft (West) = (South)
goLeft (South) = (East)
goLeft (East) = (North)

goRight :: Direction -> Direction
goRight (North) = (East)
goRight (East) = (South)
goRight (South) = (West)
goRight (West) = (North)