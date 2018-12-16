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
               | Paused
               deriving (Show, Eq)

data World = World { snake :: Snake
                   , food :: Position
                   , direction :: Direction
                   , newdir :: Direction
                   , rand :: R.StdGen
                   , limits :: (Int, Int)
                   , gameState :: GameState
                   , verticalWall :: [(Int, Int)]
                   , horizontalWall :: [(Int, Int)]
                   , score :: Int
                   } deriving (Show)

initialWorld = World { snake = [(-1, x) | x <- [(-20),(-21)..(-24)]]
                     , food = (19, 19)
                     , direction = North
                     , newdir = North
                     , rand = R.mkStdGen 0
                     , limits = (-24, 25)
                     , gameState = Playing
                     , verticalWall = generateVerticalWall initialWorld $ fst $ randomPosition (-24, 17) $ R.mkStdGen 0
                     , horizontalWall = generateHorizontalWall initialWorld $ fst $ randomPosition (-10, 17) $ R.mkStdGen 0
                     , score = 0
                     }

generateVerticalWall :: World -> (Int, Int) -> [(Int, Int)]
generateVerticalWall world a | exists [(fst a, x) | x <- [(snd a + 1),(snd a + 2)..(snd a + 7)]] (snake world) = generateVerticalWall world $ fst $ randomPosition (-24, 20) $ rand world
                             | otherwise = [(fst a, x) | x <- [(snd a + 1),(snd a + 2)..(snd a + 7)]]

generateHorizontalWall :: World -> (Int, Int) -> [(Int, Int)]
generateHorizontalWall world a | exists [(x, snd a) | x <- [(fst a + 1),(fst a + 2)..(fst a + 7)]] (snake world) = generateHorizontalWall world $ fst $ randomPosition (-24, 20) $ rand world
                             | otherwise = [(x, snd a) | x <- [(fst a + 1),(fst a + 2)..(fst a + 7)]]

exists :: Eq a => [a] -> [a] -> Bool
exists x y = any id $ (==) <$> x <*> y

--givenStringExists u theList = u `elem` map theList


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
moveSnake world | gameState world == Paused = world
                | otherwise = world { snake = (movePosition newDirection $ head (snake world)):(init (snake world)), direction = newDirection }
                where newDirection = newdir world


eat :: World -> Position -> R.StdGen -> World
eat world newFood newRand = let s = (snake world); n = (newdir world) in world { score = score world + 1, snake = (movePosition n $ head s):s, food = newFood, rand = newRand }

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
    | (gameState w) == Paused = w
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
    | inWall (movePosition dir $ head s) = go
    | outside (movePosition dir $ head s) (limits world) = go
    | otherwise = world
    where inSnake h = h `elem` s
          inWall h = h `elem` wall
          s = snake world
          wall = merge (verticalWall world) (horizontalWall world)
          dir = direction world
          go = world { gameState = GameOver  }

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys