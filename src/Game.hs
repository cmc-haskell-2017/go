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
type Cell = Maybe Stone -- переделать под data

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
  , gameBoard  = initBoard
  , listBoard = []
  }

-- | Построение пустого поля/ траблы
initBoard :: Board
initBoard  = Map.fromList(createList)

createList::[(Point2, Maybe Stone)]
createList = createListadd 0 0

createListadd:: Int->Int->[(Point2, Maybe Stone)]
createListadd i1 i2
  |i2 < boardWidth = ((i1,i2), Nothing) : createListadd i1 (i2+1)
	|i1 < boardHeight = ((i1,i2), Nothing) : createListadd (i1+1) 0
	|otherwise = []

-- =========================================
-- Отрисовка игры
-- =========================================

drawGame :: Game -> Picture
drawGame game = translate (-w) (-h) (scale c c (pictures
  [ drawGrid
  , drawBoard (gameBoard game)
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
    hs = map (\j -> line [(0, j), (n-1, j)]) [0..m-1]
    vs = map (\i -> line [(i, 0), (i, m-1)]) [0..n-1]

    n = fromIntegral boardWidth
    m = fromIntegral boardHeight

-- | Нарисовать фишки на игровом поле/ протестировать, подогнать параметры
drawBoard :: Board -> Picture
drawBoard board = pictures (map drawCells (Map.toList board))
  where
    drawCells ((x, y), cell) = translate (fromIntegral x) (fromIntegral y) (drawCell cell)

-- | Нарисовать камень, если он там есть
drawCell :: Cell -> Picture
drawCell (Just stone) = drawStone stone
drawCell Nothing = blank

-- | Нарисовать камень.
drawStone:: Stone -> Picture
drawStone Black = drawBlack
drawStone White = drawWhite

-- | Нарисовать черный камень.
drawBlack :: Picture
drawBlack = color black (Circle 0.6)

-- | Нарисовать белый камень.
drawWhite :: Picture
drawWhite = color white (Circle 0.6)


-- =========================================
-- Обработка событий
-- =========================================

-- | Обработка событий.
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) _ _ mouse) = placeStone (mouseToCell mouse)
handleGame _ = id


-- | Поставить камень и сменить игрока (если возможно).
placeStone :: Point2 -> Game -> Game -- fix
placeStone (i, j) game =
  case gameWinner game of
    Just _ -> game    -- если есть победитель, то поставить фишку нельзя
    Nothing -> case modifyAt (i, j) (gameBoard game) (gamePlayer game) of --здесь еще нужно дописать функцию преобразования
      Nothing -> game -- если поставить фишку нельзя, ничего не изменится
      Just newBoard -> game
        { gamePlayer = switchPlayer (gamePlayer game)
        , gameScore = amountScores newBoard
        , gameComi = gameComi game
        , gameWinner = winner game
        , gameBoard  = newBoard
        , listBoard = (gameBoard game) : (listBoard game)
        }

-- | Применить преобразование к элементу map
-- с заданным ключом. Если преобразование не удалось — вернуть 'Nothing'.
-- Иначе вернуть преобразованный map.
modifyAt :: Point2 -> Board -> Stone -> Maybe Board
modifyAt p board stone
  | ruleBusy p board = Nothing
  | otherwise = (Just (place p stone board))

-- | Проверка на правила игры
-- isPossible :: Point2 -> [Board] -> Board -> Stone -> Bool -- In

-- | функция равенства досок
equalBoards :: Board -> Board -> Bool
equalBoards = byKey 0 0
  where
    byKey i1 i2 a b
      | i2 < boardWidth =
        (Map.lookup (i1, i2) a) == (Map.lookup (i1, i2) b) && byKey i1 (i2+1) a b
      | i1 < boardHeight =
        (Map.lookup (i1, i2) a) == (Map.lookup (i1, i2) b) && byKey (i1+1) 0 a b
      | otherwise = True

-- | правило Ко борьбы
-- ruleKo :: Point2 -> Stone -> Board -> [Board] -> Bool
--
-- | правило свободы
-- ruleFreedom :: Point2 -> Stone -> Board -> Bool

-- | занято ли место
ruleBusy :: Point2 -> Board -> Bool
ruleBusy p board
  | Map.lookup p board == Nothing = False
  | otherwise = True

-- | убрать камни без свободы и засчитать другому игроку столько очков,
-- сколько было убрано камней
-- removeStones :: Stone -> Game -> Game

-- | поставить камень
place :: Point2 -> Stone -> Board -> Board
place p stone = Map.insert p (Just stone)

-- | сменить игрока
switchPlayer :: Stone -> Stone
switchPlayer Black = White
switchPlayer White = Black


-- | Подсчет количество очков
-- самое сложное из всей базовой части это подсчитать очки
-- над этим надо хорошенько подумать
amountScores :: Board -> Scores
amountScores _ = (0.0, 0.0)


-- | Получить координаты клетки под мышкой.
mouseToCell :: Point -> Point2
mouseToCell (x, y) = (i, j)
  where
    i = floor (x + fromIntegral screenWidth  / 2) `div` cellSize
    j = floor (y + fromIntegral screenHeight / 2) `div` cellSize

-- | Определить победителя на игровом поле
winner :: Game -> Maybe Stone
winner _ = Nothing


-- | Обновление игры.
-- В этой игре все изменения происходят только по действиям игрока,
-- поэтому функция обновления — это тождественная функция.
updateGame :: Float -> Game -> Game
updateGame _ = id

-- =========================================
-- Константы, параметры игры.
-- =========================================

-- | Начальная фора(очки) белого игрока
-- надо уточнить отрезок возмодных значений
playerComi :: Float
playerComi = 5.5

-- | начальная фора(камни) черного игрока, надо подумать как это реализовать
-- обозначает, сколько камней должен поставит черный игрок перед началом партии
-- это нужно если очковой форы не хватает и игроки слишком разного уровня
-- но не более определенного кол-ва камней, уточним потом в правилах
playerHandicap :: Int
playerHandicap = 0

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
