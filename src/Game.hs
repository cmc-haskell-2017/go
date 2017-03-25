module Game  where

import Graphics.Gloss.Interface.Pure.Game
import Data.Map (Map)
import qualified Data.Map as Map

run :: IO ()
run = do
    play display bgColor fps initGame drawGame handleGame updateGame
  where
    display = InWindow "Game Go" (screenWidth, screenHeight) (200, 200)
    bgColor = makeColorI 245 245 220 255 -- цвет фона
    fps     = 60      -- кол-во кадров в секунду


-- памятка
--play :: Display
  -- -> Color
  -- -> Int
  -- -> world -- world - переменная типа
  -- -> (world -> Picture) -- view
  -- -> (Event -> world -> world) - обработка событий(нажатие на кнопку мышки)
  -- -> (Float -> world -> world) - обновление для каждого кадра
  -- -> IO ()


-- функция отрисовки и функция победителя.
-- =========================================
-- Модель игры
-- =========================================

-- | Модель камня игрока
data Stone = Black | White
  deriving(Eq, Show)

-- | Клетка поля, но это не точно
data Cell = Maybe Stone

-- | Количество очков для одного игрока
type Score = Float

-- | Количество очков двух игроков
type Scores = (Score, Score)

-- | Наша точка
type Point2 = (Int, Int)

-- | Игровое поле
type Board = Map Point2 Cell

-- | состоние поля, мне кажется оно должно быть таким
data Game = Game
  { gamePlayer :: Stone -- чей ход
  , gameScore :: Scores -- количества очков для первого и второго игрока
  , gameComi :: Float -- колличество форы
  , gameWinner :: Maybe Stone -- победитель?!
  , gameBoard :: Board
  , listBoard :: [Board] -- список всех предыдущих состояний
  }

-- | Начальное состояние игры.
-- Игровое поле — пусто.
-- Первый игрок ходит черными.
initGame :: Game
initGame = Game
  { gamePlayer = Black
  , gameScore = (0, 0)
  , gameComi = playerComi
  , gameWinner = Nothing
  -- , gameBoard  = replicate boardHeight (replicate boardWidth Nothing)
  }

-- =========================================
-- Отрисовка игры
-- =========================================

drawGame :: Game -> Picture
drawGame game = translate (-w) (-h) (scale c c (pictures
  [ drawGrid
  , blank
  ]))
  where
    c = fromIntegral cellSize
    w = fromIntegral screenWidth  / 2 - offset
    h = fromIntegral screenHeight / 2 - offset
    offset = fromIntegral screenOffset / 2

-- | Сетка игрового поля.
drawGrid :: Picture
drawGrid = color black (pictures (hs ++ vs))
  where
    hs = map (\j -> line [(0, j), (n, j)]) [0..m]
    vs = map (\i -> line [(i, 0), (i, m)]) [0..n]

    n = fromIntegral boardWidth
    m = fromIntegral boardHeight

-- | Нарисовать фишки на игровом поле.
-- drawBoard :: Maybe Mark -> Board -> Picture

-- | Нарисовать камень
-- drawCell :: (Int, Int) -> Maybe Stone -> Cell -> Picture


-- | Нарисовать камень.
-- drawStone:: Stone -> Picture
-- drawStone Black = drawX
-- drawStone White = drawO
--
-- -- | Нарисовать «крестик».
-- drawX :: Picture
-- drawX = pictures
--   [ polygon [(-0.4,  0.3), (-0.3,  0.4), ( 0.4, -0.3), ( 0.3, -0.4)]
--   , polygon [(-0.4, -0.3), (-0.3, -0.4), ( 0.4,  0.3), ( 0.3,  0.4)] ]
--
-- -- | Нарисовать «нолик».
-- drawO :: Picture
-- drawO = thickCircle 0.3 0.1


-- =========================================
-- Обработка событий
-- =========================================

-- | Обработка событий.
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) _ _ mouse) = placeStone (mouseToCell mouse)
handleGame _ = id


-- | Поставить камень и сменить игрока (если возможно).
placeStone :: Point2 -> Game -> Game -- fix
placeStone _ x = x

-- | Проверка на правила игры
-- isPossible :: Point2 -> [Board] -> Board -> Stone -> Bool -- In


-- | функция равенства досок
-- equalBoard :: Board -> Board -> Bool

-- | правило Ко борьбы
-- ruleKo :: Point2 -> Stone -> Board -> [Board] -> Bool
--
-- | правило свободы
-- ruleFreedom :: Point2 -> Stone -> Board -> Bool

-- | занято ли место
-- ruleBusy :: Point2 -> Board -> Bool

-- | убрать камни без свободы и засчитать другому игроку столько очков,
-- сколько было убрано камней
-- removeStones :: Stone -> Game -> Game

-- | поставить камень
-- place :: Point2 -> Stone -> Board -> Board

-- | сменить игрока
-- switch :: Stone -> Stone

-- | Подсчет количество очков
-- amountScores :: Board -> Scores
-- amountScores _ _ = (0.0, 0.0)


-- | Получить координаты клетки под мышкой.
mouseToCell :: Point -> Point2
mouseToCell (x, y) = (i, j)
  where
    i = floor (x + fromIntegral screenWidth  / 2) `div` cellSize
    j = floor (y + fromIntegral screenHeight / 2) `div` cellSize

-- | Определить победителя на игровом поле
-- winner :: Game -> Maybe Stone


-- | Обновление игры.
-- В этой игре все изменения происходят только по действиям игрока,
-- поэтому функция обновления — это тождественная функция.
updateGame :: Float -> Game -> Game
updateGame _ = id

-- =========================================
-- Константы, параметры игры.
-- =========================================

-- | Начальная фора белого игрока
playerComi :: Float
playerComi = 6.5

-- | Ширина игрового поля в клетках.
boardWidth :: Int
boardWidth  = 9

-- | Высота игрового поля в клетках.
boardHeight :: Int
boardHeight = 9

-- | Размер одной клетки в пикселях.
cellSize :: Int
cellSize = 50

-- | отступы от края экрана
screenOffset :: Int
screenOffset = 40

-- | Ширина экрана в пикселях.
screenWidth :: Int
screenWidth  = cellSize * boardWidth + screenOffset

-- | Высота экрана в пикселях.
screenHeight :: Int
screenHeight = cellSize * boardHeight + screenOffset
