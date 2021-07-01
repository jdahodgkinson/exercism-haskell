module Robot
  ( Bearing (East, North, South, West),
    bearing,
    coordinates,
    mkRobot,
    move,
  )
where

data Bearing
  = North
  | East
  | South
  | West
  deriving (Eq, Show)

type Coordinates = (Integer, Integer)

data Robot = Robot Bearing Coordinates

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> Coordinates
coordinates (Robot _ c) = c

mkRobot :: Bearing -> Coordinates -> Robot
mkRobot = Robot

move :: Robot -> String -> Robot
move robot [] = robot
move robot (c : cs) = move (instruction robot) cs
  where
    instruction :: Robot -> Robot
    instruction = case c of
      'A' -> advance
      'L' -> turnLeft
      'R' -> turnRight
      _ -> id

turnLeft :: Robot -> Robot
turnLeft (Robot b c) = Robot newB c
  where
    newB = case b of
      North -> West
      East -> North
      South -> East
      West -> South

turnRight :: Robot -> Robot
turnRight (Robot b c) = Robot newB c
  where
    newB = case b of
      North -> East
      East -> South
      South -> West
      West -> North

advance :: Robot -> Robot
advance (Robot b (x, y)) = Robot b newC
  where
    newC = case b of
      North -> (x, y + 1)
      East -> (x + 1, y)
      South -> (x, y - 1)
      West -> (x - 1, y)
