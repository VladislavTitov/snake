module Types where

import qualified System.Random as R

data Direction = North
               | South
               | East
               | West
               deriving (Show, Eq)

--data Command = Quit | Go Direction deriving (Show, Eq)

type Position = (Int, Int)
type Snake = [Position]

data GameState = Playing
               | GameOver
               deriving (Show, Eq)

data World = World { snake :: Snake
                   , food :: Position
                   , direction :: Direction
                   , newdir :: Direction
                   , rand :: R.StdGen
                   , limits :: (Int, Int)
                   , gameState :: GameState
                   } deriving (Show)

initialWorld = World { snake = [(-1, x) | x <- [(-20),(-21)..(-24)]]
                     , food = (19, 19)
                     , direction = North
                     , newdir = North
                     , rand = R.mkStdGen 0
                     , limits = (-24, 25)
                     , gameState = Playing
                     }

opposite :: Direction -> Direction
opposite d = case d of
    North -> South
    South -> North
    East -> West
    West -> East

movePosition :: Direction -> Position -> Position
movePosition d (x, y) = case d of
    North -> (x, y + 1)
    South -> (x, y - 1)
    East -> (x + 1, y)
    West -> (x - 1, y)

moveSnake :: World -> World
moveSnake world = world { snake = (movePosition newDirection $ head (snake world)):(init (snake world)), direction = newDirection } 
  where newDirection = newdir world


eat :: World -> Position -> R.StdGen -> World
eat world newFood newRand = let s = (snake world); n = (newdir world) in world { snake = (movePosition n $ head s):s, food = newFood, rand = newRand }

removeOpposite :: World -> World
removeOpposite world
	| (newdir world) == opposite (direction world) = world {newdir = (direction world)}
	| otherwise = world

randomPosition :: R.RandomGen g => (Int, Int) -> g -> (Position, g)
randomPosition (maxr, maxc) g =
    let (r, g1) = R.randomR (1, maxr) g
        (c, g2) = R.randomR (1, maxc) g1
    in ((r, c), g2)

randomFreePosition :: R.RandomGen g => (Int, Int) -> g -> Snake -> (Position, g)
randomFreePosition lim g s =
    head $ dropWhile inSnake (randomPositions g)
    where inSnake (x, _) = x `elem` s
          randomPositions h = r:randomPositions g'
              where r@(_, g') = randomPosition lim h


advance :: World -> World
advance w
    | (movePosition newDir $ head $ snake w) == (food w) = eaten
    | otherwise = slithered
    where newDir = newdir w
          slithered = moveSnake w 
          eaten = eat w newFood newRand 
          (newFood, newRand) = randomFreePosition (limits w) (rand w) $ snake eaten
		  
ins :: Position -> (Int, Int) -> Bool
ins (x, y) (limMin, limMax) = limMin <= x && x <= limMax && limMin <= y && y <= limMax

outside p l = not $ ins p l

handleCollision :: World -> World
handleCollision world
    | inSnake (movePosition dir $ head s) = go
    | outside (movePosition dir $ head s) (limits world) = go
    | otherwise = world
    where inSnake h = h `elem` s
          s = snake world
          dir = direction world
          go = world { gameState = GameOver  }
