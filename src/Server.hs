module Server  where

import Graphics.Gloss.Interface.Pure.Game

run :: IO ()
run = do
    play display bgColor fps initGame drawGame handleGame updateGame
  where
    display = InWindow "Game Go" (screenWidth, screenHeight) (200, 200)
    bgColor = green   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

    -- drawGame x = blank
    updateGame _ x = x	

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

type Point2 = (Int, Int)

-- | Над игровым полем еще надо подумать
-- type Board = Map Point2 Cell
type Board = [[Cell]]

-- | состоние поля, мне кажется оно должно быть таким 
data Game = Game
  { gamePlayer :: Stone -- чей ход
  , gameScore :: Scores -- количества очков для первого и второго игрока
  , gameComi :: Float -- колличество форы
  , gameWinner :: Maybe Stone -- победитель?!
  , gameBoard :: Board
  , listBoard :: [Board]
  -- список всех предыдущих состояний
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
drawGame x = blank


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


-- функция равенства досок
-- под каждое правило своя функция

-- поставить камень
-- place :: Point2 -> Stone -> Board -> Board

-- сменить игрока
-- switch :: Stone -> Stone

-- | Подсчет количество очков
amountScores :: Board -> Scores
amountScores _ _ = (0.0, 0.0)


-- | Получить координаты клетки под мышкой.
mouseToCell :: Point -> Point2
mouseToCell (x, y) = (i, j)
  where
    i = floor (x + fromIntegral screenWidth  / 2) `div` cellSize
    j = floor (y + fromIntegral screenHeight / 2) `div` cellSize

-- =========================================
-- Константы, параметры игры. 
-- =========================================

-- | Начальная фора белого игрока
playerComi :: Float
playerComi = 6.5

-- | Ширина игрового поля в клетках.
boardWidth :: Int
boardWidth  = 19

-- | Высота игрового поля в клетках.
boardHeight :: Int
boardHeight = 19

-- | Размер одной клетки в пикселях.
cellSize :: Int
cellSize = 50

-- | Ширина экрана в пикселях.
screenWidth :: Int
screenWidth  = cellSize * boardWidth

-- | Высота экрана в пикселях.
screenHeight :: Int
screenHeight = cellSize * boardHeight


