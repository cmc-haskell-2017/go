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
data Cell = Cell Stone | Empty -- переделать под data
  deriving(Eq, Show)

-- | Количество очков для одного игрока
type Score = Float

-- | Количество очков двух игроков
type Scores = (Score, Score)

-- | Наша точка
type Point2 = (Int, Int)

-- | Игровое поле
type Board = Map Point2 Cell

-- | кол-во камней, которые "съели" черные
type Blacksum = Int

-- | кол-во камней, которые "съели" белые
type Whitesum = Int

-- | кол-во камней убранных каждым игроком
type AmountStones = (Blacksum, Whitesum)

-- | состоние поля, мне кажется оно должно быть таким
data Game = Game
  { gamePlayer :: Stone -- чей ход
  , gameScore :: Scores -- количества очков для первого и второго игрока
  , gameComi :: Float -- колличество форы
  , gameWinner :: Maybe Stone -- победитель?!
  , gameBoard :: Board
  , listBoard :: [Board] -- список всех предыдущих состояний
  , scoreStones :: AmountStones -- кол-во камней убранных каждым игроком
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
  , scoreStones = (0, 0)
  }

-- | Построение пустого поля/ траблы
initBoard :: Board
initBoard  = Map.fromAscList(createList)

createList:: [(Point2, Cell)]
createList = createListadd 0 0

createListadd:: Int -> Int -> [(Point2, Cell)]
createListadd i1 i2
  |i2 < boardWidth = ((i1,i2), Empty) : createListadd i1 (i2 + 1)
  |i1 < boardHeight - 1 = createListadd (i1 + 1) 0
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
drawCell Empty = blank
drawCell (Cell stone) = drawStone stone


-- | Нарисовать камень.
drawStone:: Stone -> Picture
drawStone Black = drawBlack
drawStone White = drawWhite

-- | Нарисовать черный камень.
drawBlack :: Picture
drawBlack = color black (circleSolid 0.45)

-- | Нарисовать белый камень.
drawWhite :: Picture
drawWhite = pictures
  [color white (circleSolid 0.45)
  , color black (circle 0.45)
  ]


-- =========================================
-- Обработка событий
-- =========================================

-- | Обработка событий.
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) _ _ mouse) = placeStone (mouseToCell mouse)
handleGame _ = id

-- | Получить координаты клетки под мышкой.
mouseToCell :: Point -> Maybe Point2
mouseToCell (x, y) = (Just (i, j))
  where
    i = floor (x + fromIntegral screenWidth  / 2) `div` cellSize
    j = floor (y + fromIntegral screenHeight / 2) `div` cellSize

-- | Поставить камень и сменить игрока (если возможно).
placeStone :: Maybe Point2 -> Game -> Game -- fix
placeStone (Just point) game =
    case gameWinner game of
      Just _ -> game    -- если есть победитель, то поставить фишку нельзя
      Nothing -> case modifyAt point (gameBoard game) (gamePlayer game) (listBoard game) of --здесь еще нужно дописать функцию преобразования
        Nothing -> game -- если поставить фишку нельзя, ничего не изменится
        Just newBoard -> game
          { gamePlayer = switchPlayer (gamePlayer game)
          , gameScore = amountScores newBoard
          , gameComi = gameComi game
          , gameWinner = winner game
          , gameBoard  = newBoard
          , listBoard = (gameBoard game) : (listBoard game)
          , scoreStones = (0, 0)
          }

-- | Применить преобразование к элементу map
-- с заданным ключом. Если преобразование не удалось — вернуть 'Nothing'.
-- Иначе вернуть преобразованный map.
modifyAt :: Point2 -> Board -> Stone -> [Board] -> Maybe Board
modifyAt point board stone boards
  | isPossible point board stone boards = (Just (place point stone board))
  | otherwise = Nothing
  -- | ruleBusy p board = Nothing -- здесь должна быть функция isPossible
  -- | otherwise = (Just (place p stone board))

-- | Проверка на правила игры
isPossible :: Point2 -> Board -> Stone -> [Board] -> Bool
isPossible point board _ _ -- два последних параметра нам не нужны, пока не доделали все функции правил
  | ruleBusy point board = False
  | otherwise = True

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
  | Map.lookup p board == (Just Empty) = False
  | otherwise = True

-- | убрать камни без свободы и засчитать другому игроку столько очков,
-- сколько было убрано камней
-- removeStones :: Stone -> Game -> Game

-- | удаление мертвых камней при окончании игры
-- Например, когда игра закончилась(оба игрока с пасовали), то остались группы камней,
-- у которых еще есть свобода, но при этом они считаются мертвыми, потому что
-- другой игрок может убрать их за пару ходов. Если у этих камней есть возможность поставить два глаза,
-- то они не могут быть убраны.
--removeDead :: Game -> Game

-- | поставить камень
place :: Point2 -> Stone -> Board -> Board
place p stone = Map.insert p (Cell stone)

-- | сменить игрока
switchPlayer :: Stone -> Stone
switchPlayer Black = White
switchPlayer White = Black


-- | Подсчет количество очков
-- самое сложное из всей базовой части это подсчитать очки
-- над этим надо хорошенько подумать
amountScores :: Board -> Scores
amountScores _ = (0.0, 0.0)

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
-- может варьироваться уже по желания, но это позже в индивидуальных частях
playerComi :: Float
playerComi = 6.5

-- | начальная фора(камни) черного игрока, надо подумать как это реализовать
-- обозначает, сколько камней должен поставит черный игрок перед началом партии
-- это нужно если очковой форы не хватает и игроки слишком разного уровня
-- но не более 9(для доски 19x19), для 9x9 будет не больше 3
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
screenOffset = 50

-- | Ширина экрана в пикселях.
screenWidth :: Int
screenWidth  = cellSize * boardWidth + screenOffset

-- | Высота экрана в пикселях.
screenHeight :: Int
screenHeight = cellSize * boardHeight + screenOffset
