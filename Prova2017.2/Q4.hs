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

faces :: Direction -> [Command] -> Direction
faces dir [] = dir
faces dir ((TurnLeft):cms) = faces (goLeft dir) cms
faces dir ((TurnRight):cms) = faces (goRight dir) cms 
faces dir (_:cms) = faces dir cms

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