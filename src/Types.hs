module Types where

import qualified System.Random as R

data Direction = North
               | South
               | East
               | West
               deriving (Show, Eq)

data Command = Quit | Go Direction deriving (Show, Eq)

type Position = (Int, Int)
type Snake = [Position]

data World = World { snake :: Snake
                   , food :: Position
                   , direction :: Direction
                   , rand :: R.StdGen
                   , limits :: (Int, Int)
                   } deriving (Show)

data GameState = Playing World
               | GameOver
               deriving (Show)


initialWorld = World { snake = [(-1, x) | x <- [(-20),(-21)..(-24)]]
                     , food = (19, 19)
                     , direction = North
                     , rand = R.mkStdGen 0
                     , limits = (-24, 25)
                     }

movePosition :: Direction -> Position -> Position
movePosition d (x, y) = case d of
    North -> (x, y + 1)
    South -> (x, y - 1)
    East -> (x + 1, y)
    West -> (x - 1, y)

moveSnake :: Direction -> World -> World
moveSnake newDirection world = world { snake = (movePosition newDirection $ head (snake world)):(init (snake world)), direction = newDirection }

