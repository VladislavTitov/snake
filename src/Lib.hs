module Lib
    ( appMain
    ) where

import Graphics.Gloss
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






window :: Display
window = InWindow "Snake" (fieldW, fieldH) (100, 100)

background :: Color
background = black 

fps = 60

drawing :: Picture
drawing = translate x y $ color white $ rectangleSolid cellW cellH
	where (x, y) = fromGameCoords (25, 25)

appMain :: IO ()
appMain = play window background fps 1 (\s -> drawing) (\_ a -> a) (\_ a -> a) 
