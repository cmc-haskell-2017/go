module Server  where

import Graphics.Gloss.Interface.Pure.Game

run :: IO ()
run = do
    play display bgColor fps initGame drawGame handleGame updateGame
  where
    display = InWindow "Game Go" (screenWidth, screenHeight) (200, 200)
    bgColor = green   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

    drawGame x = blank
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

-- | Над игровым полем еще надо подумать
type Board = [[Cell]]

-- | состоние поля, мне кажется оно должно быть таким 
data Game = Game
  { gamePlayer :: Stone -- чей ход
  , gameScore :: Scores -- количества очков для первого и второго игрока
  , gameComi :: Float -- колличество форы
  , gameWinner :: Maybe Stone -- победитель?!
    --GameBoard :: Board
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


-- =========================================
-- Обработка событий
-- =========================================

-- | Обработка событий.
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) _ _ mouse) = placeStone (mouseToCell mouse)
handleGame _ = id


-- | Поставить камень и сменить игрока (если возможно).
placeStone :: (Int, Int) -> Game -> Game
placeStone _ x = x

-- | Проверка на правила игры


-- | Подсчет количество очков
amountScores :: Board -> Game -> Scores
amountScores _ _ = (0, 0)


-- | Получить координаты клетки под мышкой.
mouseToCell :: Point -> (Int, Int)
mouseToCell (x, y) = (i, j)
  where
    i = floor (x + fromIntegral screenWidth  / 2) `div` cellSize
    j = floor (y + fromIntegral screenHeight / 2) `div` cellSize

-- =========================================
-- Константы, параметры игры. По началу, потом будет переменные величины
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


