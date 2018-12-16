module Lib
    ( appMain
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Types

type GPosition = (Float, Float) -- Аналог типа Позиция для Глосса (отличие в типах кортежа)

fieldW = 500 -- ширина игровой области
fieldH = 500 -- высота ~

-- | Игровое поле делится на ячейки 
-- Из этого следует, что мы получаем две системы координат, игровую и графическую
-- Графическая считается в глоссовских единицах (Float) 
-- Игровая в определенных нами единицах (Int)

cellW = 10 -- ^ ширина ячейки
cellH = 10 -- ^ высота ~

-- | Переводит игровые координаты в графические 
-- По дефолту область определения игровых координат [-24, 25],
-- а графических - [-250, 250]

fromGameCoords :: Position -> GPosition
fromGameCoords (x, y) = (fromIntegral x * cellW - (cellW / 2), fromIntegral y * cellH - (cellH / 2))

drawPosition :: Position -> Color -> Picture
drawPosition pos col = translate x y $ color col $ rectangleSolid cellW cellH
  where (x, y) = fromGameCoords pos

foodColor :: Color
foodColor = green

snakeColor :: Color
snakeColor = white

wallColor :: Color
wallColor = yellow

renderSnake :: World -> Picture
renderSnake world = pictures [drawPosition p snakeColor | p <- snake world]

renderFood :: World -> Picture
renderFood world = drawPosition (food world) foodColor

renderWall :: World -> Picture
renderWall world = pictures [drawPosition p wallColor | p <- merge (verticalWall world) (horizontalWall world)]

render :: World -> Picture
render world 
            | (gameState world) == GameOver = pictures 
                                              [ blank
                                              , translate (-140) (7) $ scale 0.4 0.4 $ color red $ text ("Game Over")
                                              , translate (-140) (-30) $ scale 0.3 0.3 $ color green $ text ("Your score: " ++ show (score world))
                                              ]
            | (gameState world) == Paused = pictures
                                            	            [ renderSnake world
                                            						  , renderFood world
                                            						  , renderWall world
                                            						  , translate (-110) (-20) $ scale 0.5 0.5 $ color blue $ text "Paused"
                                            						  ]
            | otherwise = pictures
	            [ renderSnake world
						  , renderFood world
						  , renderWall world
						  ]

update :: Float -> World -> World
update _ world = (advance $ handleCollision world)

handleInput :: Event -> World -> World
handleInput (EventKey (Char 'p') _ _ _) world = world {gameState = Paused}
handleInput (EventKey (Char 's') _ _ _) world
 |  (gameState world) == GameOver = initialWorld
 |  otherwise = world {gameState = Playing}

handleInput (EventKey (SpecialKey KeyUp) _ _ _) world
  | (direction world) == North = update 0 world
  | otherwise = removeOpposite $ world {newdir = North}

handleInput (EventKey (SpecialKey KeyDown) _ _ _) world
  | (direction world) == South = update 0 world
  | otherwise = removeOpposite $ world {newdir = South}

handleInput (EventKey (SpecialKey KeyLeft) _ _ _) world
  | (direction world) == West = update 0 world
  | otherwise = removeOpposite $ world {newdir = West}

handleInput (EventKey (SpecialKey KeyRight) _ _ _) world
  | (direction world) == East = update 0 world
  | otherwise = removeOpposite $ world {newdir = East}

handleInput _ world = world

window :: Display
window = InWindow "Snake" (fieldW, fieldH) (100, 100)

background :: Color
background = black 

fps = 5

appMain :: IO ()
appMain = play window background fps initialWorld render handleInput update
