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


initialWorld = World { snake = [(-1, x) | x <- [4,3..0]]
                     , food = (19, 19)
                     , direction = North
                     , rand = R.mkStdGen 0
                     , limits = (-24, 25)
                     }
