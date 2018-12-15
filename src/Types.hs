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

