module Draw where

import Graphics.Gloss.Interface.Pure.Game
import Config
import Models

import Data.Char
import qualified Data.Map as Map

-- =========================================
-- Отрисовка игры
-- =========================================

-- | Отрисовка игры, складывает изображения сетки и поля.
drawGame :: Game -> Picture
drawGame game = translate (-w) (-h) (scale c c (pictures
  [ drawGrid
  , drawBoard (gameBoard game)
  -- , drawPass (numberOfPass game)
  , drawScores (gameScore game)
  -- , drawStones (scoreStones game)
  , drawEndGame (gameWinner game) (scoreStones game) (endGame game)
  ]))
  where
    c = fromIntegral cellSize
    w = fromIntegral screenWidth  / 2 - offset
    h = fromIntegral screenHeight / 2 - offset
    offset = fromIntegral screenOffset / 2

-- | Отрисовка количества пассов
drawPass :: Passes -> Picture
drawPass (x,y) = (scale 0.005 0.005 (text [(intToDigit x), (intToDigit y)]))

-- | Отрисовка количества очков игры
drawScores :: Scores -> Picture
drawScores (x, y) = translate w h (scale 0.005 0.005 (text [(intToDigit (round x)), (intToDigit  (round y))] ))
  where
    w = fromIntegral screenWidth / 200
    h = fromIntegral screenHeight / 200

drawEndGame :: Maybe Stone -> AmountStones -> Maybe Float -> Picture
drawEndGame _ _ Nothing = blank
drawEndGame _ _ (Just c) = (scale c c (text "END GAME"))

--
drawStones :: AmountStones -> Picture
drawStones (x, y) = translate w h (scale 0.005 0.005 (text [(intToDigit x), (intToDigit y)] ))
  where
    w = fromIntegral screenWidth / 200
    h = fromIntegral screenHeight / 200

-- | Ортисовка сетки игрового поля.
drawGrid :: Picture
drawGrid = color black (pictures (hs ++ vs))
  where
    hs = map (\j -> line [(0, j), (n - 1, j)]) [0..m - 1]
    vs = map (\i -> line [(i, 0), (i, m - 1)]) [0..n - 1]

    n = fromIntegral boardWidth
    m = fromIntegral boardHeight

-- | Нарисовать фишки на игровом поле.
drawBoard :: Board -> Picture
drawBoard board = foldMap drawCells (Map.toList board)
  where
    drawCells ((x, y), cell) = translate (fromIntegral x) (fromIntegral y) (drawCell cell)

-- | Нарисовать камень, если он там есть.
drawCell :: Cell -> Picture
drawCell Empty = blank
drawCell (Cell stone) = drawStone stone
drawCell (CellShadow stone) = drawShadowStone stone

-- | Нарисовать камень.
drawStone :: Stone -> Picture
drawStone Black = drawBlack
drawStone White = drawWhite

-- | Разымытие камня.
drawShadowStone :: Stone -> Picture
drawShadowStone Black = drawShadowBlack
drawShadowStone White = drawShadowWhite

-- | Размытие черного камня.
drawShadowBlack :: Picture
drawShadowBlack =  color (withAlpha 0.5 black) (circleSolid radiusStone)

-- | Размытие белого камня.
drawShadowWhite :: Picture
drawShadowWhite = pictures
  [color (withAlpha 0.5 white) (circleSolid radiusStone)
  , color (withAlpha 0.5 black) (circle radiusStone)
  ]

-- | Нарисовать черный камень.
drawBlack :: Picture
drawBlack = color black (circleSolid radiusStone)

-- | Нарисовать белый камень, с черной каймой.
drawWhite :: Picture
drawWhite = pictures
  [color white (circleSolid radiusStone)
  , color black (circle radiusStone)
  ]
