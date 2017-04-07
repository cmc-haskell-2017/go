module Game  where
import Data.Monoid
import Data.Foldable

import Graphics.Gloss.Interface.Pure.Game
import Data.Map (Map)
import qualified Data.Map as Map

run :: IO ()
run = do
    play display bgColor fps initGame drawGame handleGame updateGame
  where
    display = InWindow "Game Go" (screenWidth, screenHeight) (200, 200)
    bgColor = makeColorI 245 245 220 255 -- цвет фона, бежевый
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

-- | Модель камня игрока.
data Stone = Black | White
  deriving(Eq, Show)

-- | Клетка поля.
data Cell = Cell Stone | CellShadow Stone | Empty
  deriving(Eq, Show)

-- | Количество очков для одного игрока.
type Score = Float

-- | Количество очков двух игроков.
type Scores = (Score, Score)

-- | Наша точка.
type Node = (Int, Int)

-- | Игровое поле.
type Board = Map Node Cell

-- | Кол-во камней, которые "съели" черные.
type Blacksum = Int

-- | Кол-во камней, которые "съели" белые.
type Whitesum = Int

-- | Кол-во камней убранных каждым игроком.
type AmountStones = (Blacksum, Whitesum)

-- | Состоние поля, мне кажется оно должно быть таким.
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

-- | Построение пустого поля.
initBoard :: Board
initBoard  = Map.fromAscList createList

-- | Начало поля с точки (0, 0).
createList:: [(Node, Cell)]
createList = createListadd 0 0

-- | Идет рекурсивно по столбцам и строкам и заполняет список пустыми клетками.
createListadd:: Int -> Int -> [(Node, Cell)]
createListadd i1 i2
  |i2 < boardWidth = ((i1, i2), Empty) : createListadd i1 (i2 + 1)
  |i1 < boardHeight - 1 = createListadd (i1 + 1) 0
  |otherwise = []

-- =========================================
-- Отрисовка игры
-- =========================================

-- | Отрисовка игры, складывает изображения сетки и поля.
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
drawStone:: Stone -> Picture
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

-- =========================================
-- Обработка событий
-- =========================================

-- | Обработка событий.
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) _ _ mouse) = placeStone (mouseToCell mouse)
handleGame (EventMotion mouse) = placeShadowStone (mouseToCell mouse)
handleGame _ = id

-- | Поставить размытый камень под курсором мышки.
placeShadowStone :: Maybe Node -> Game -> Game
placeShadowStone Nothing game = game
placeShadowStone (Just point) game
  | ruleBusy point (gameBoard game) = game
  |otherwise = game {gameBoard = Map.insert point (CellShadow ( gamePlayer game)) (deleteShadows (gameBoard game)) }

-- | Убрать размытый камень, когда курсора нет.
deleteShadows :: Board -> Board
deleteShadows board = (Map.fromList (map (\(p, a) -> if ((a == (CellShadow Black)) || (a == (CellShadow White))) then (p, Empty) else (p, a)) (Map.toList board)))

-- | Получить координаты клетки под мышкой(потом будет распознование близости мышки с пересечением сетки).
mouseToCell :: Point -> Maybe Node
mouseToCell (x, y) = (Just (i, j))
  where
    i = floor (x + fromIntegral screenWidth  / 2) `div` cellSize
    j = floor (y + fromIntegral screenHeight / 2) `div` cellSize

-- | Поставить камень и сменить игрока (если возможно).
placeStone :: Maybe Node -> Game -> Game
placeStone Nothing game = game
placeStone (Just point) game =
    case gameWinner game of
      Just _ -> game    -- если есть победитель, то поставить фишку нельзя
      Nothing -> case modifyAt point (gameBoard game) (gamePlayer game) (listBoard game) of --здесь еще нужно дописать функцию преобразования
        Nothing -> game -- если поставить фишку нельзя, ничего не изменится
        Just newBoard -> removeStones  (completeMove newBoard game)

-- | Закончить ход, инициализировать состояние игры для нового хода.
completeMove :: Board -> Game -> Game
completeMove board game = game
  { gamePlayer = switchPlayer (gamePlayer game)
  , gameScore = amountScores board
  , gameWinner = winner game
  , gameBoard  = board
  , listBoard = setBoard board (listBoard game)
  }

-- | История состояний игрового поля.
setBoard :: Board -> [Board] -> [Board]
setBoard board listboard = board : listboard


-- | Применить преобразование к элементу map
-- с заданным ключом. Если преобразование не удалось — вернуть 'Nothing'.
-- Иначе вернуть преобразованный map.
modifyAt :: Node -> Board -> Stone -> [Board] -> Maybe Board
modifyAt point board stone boards
  | isPossible point board stone boards = (Just (place point stone board))
  | otherwise = Nothing

-- | Проверка на правила игры, false если не по правилам.
isPossible :: Node -> Board -> Stone -> [Board] -> Bool
isPossible point board stone listboard
  | ruleBusy point board = False
  | (not (ruleKo point stone board listboard)) = False
  | (not (ruleFreedom point stone board)) = False
  | otherwise = True


-- | Правило Ко борьбы, true если все по правилам.
-- Конкретно данное состояние встретилось менее трех раз и оно не совпало с предыдущим => все норм
ruleKo :: Node -> Stone -> Board -> [Board] -> Bool
ruleKo _ _ _ [] = True
ruleKo point stone board (x:xs)
  = ammEqBoards board (x:xs) < 3
  && place point stone board /= x

-- | Сколько раз встречалась такая доска раньше.
ammEqBoards :: Board -> [Board] -> Int
ammEqBoards board = length . filter (== board)

-- | Правило свободы.
-- | Возвращает True, если все ок (можно ставить камень).
ruleFreedom :: Node -> Stone -> Board -> Bool
ruleFreedom (point_row, point_col) stone board = cmpFieldWithEmpty (point_row-1) point_col board stone ||
                                                 cmpFieldWithEmpty (point_row+1) point_col board stone ||
                                                 cmpFieldWithEmpty point_row (point_col-1) board stone ||
                                                 cmpFieldWithEmpty point_row (point_col+1) board stone

-- | Возвращает True если правило свободы выполненно для данного соседа
cmpFieldWithEmpty:: Int->Int->Board->Stone->Bool
cmpFieldWithEmpty point_row point_col board stone
        | borderCmp point_row point_col = False
        | otherwise = (Map.lookup (point_row, point_col) board) == (Just Empty) ||
                      (sizeList(delSimil(noLastFree point_row point_col board stone [])) > 1)

-- \ Размер списка
sizeList::[a]->Int
sizeList [] = 0
sizeList (_ : xs) = 1 + sizeList xs

-- \ Удаление одинаковых элемнтов списка
delSimil::[(Int,Int)]->[(Int,Int)]
delSimil [] = []
delSimil ((x,y):xs) = (x,y) : delSimil (filter (\(x2,y2)->x2/=x && y2/=y) xs)

-- | Возвращает список из координат свободных позиций вокруг группы камней (с повторениями)
noLastFree::Int->Int->Board->Stone->[(Int,Int)]->[(Int,Int)]
noLastFree row col board stone list
  | borderCmp row col || ((filter (\(x,y)->x==row && y==col) list) /= [])= []
  | (Map.lookup (row, col) board) == (Just Empty) = [(row,col)]
  | (Map.lookup (row, col) board) == (Just (Cell stone)) = noLastFree (row - 1) col board stone ((row,col) : list) ++
                                                           noLastFree (row + 1) col board stone ((row,col) : list) ++
                                                           noLastFree row (col - 1) board stone ((row,col) : list) ++
                                                           noLastFree row (col + 1) board stone ((row,col) : list)
  | otherwise = []

noLastFreeAmount:: Int -> Int -> Board -> Stone -> [(Int,Int)] -> Int
noLastFreeAmount row col board stone list
  | borderCmp row col = 0
  | (filter (\(x, y) -> x == row && y == col) list) /= [] = 0
  | (Map.lookup (row, col) board) == (Just Empty) = 1
  | (Map.lookup (row, col) board) == (Just (Cell stone)) = noLastFreeAmount (row - 1) col board stone ((row, col) : list) +
                                                           noLastFreeAmount (row + 1) col board stone ((row, col) : list) +
                                                           noLastFreeAmount row (col - 1) board stone ((row, col) : list) +
                                                           noLastFreeAmount row (col + 1) board stone ((row, col) : list)
  | otherwise = 0
  -- | Возвращает True если вышли за границы поля
borderCmp::Int->Int->Bool
borderCmp point_row point_col = point_row < 0 ||
                                point_row > boardHeight ||
                                point_col < 0 ||
                                point_col > boardWidth

-- | Занято ли место, false если не занято.
ruleBusy :: Node -> Board -> Bool
ruleBusy p board
  | Map.lookup p board == (Just Empty) || Map.lookup p board == (Just (CellShadow Black)) || Map.lookup p board == (Just (CellShadow White)) = False
  | otherwise = True

-- | Убрать камни без свободы и засчитать другому игроку столько очков,
-- сколько было убрано камней.
removeStones :: Game -> Game
removeStones game = Game
  { gamePlayer = gamePlayer game
  , gameScore = gameScore game
  , gameComi = gameComi game
  , gameWinner = gameWinner game
  , gameBoard  = (deleteFromBoard loS (gameBoard game))
  , listBoard = listBoard game
  , scoreStones = countStones (scoreStones game)  loS
  }
    where
      loS = listOfStones (Map.toList (gameBoard game)) (gameBoard game)

-- Возвращает список камней которые, необходимо удалить.
listOfStones :: [(Node, Cell)] -> Board -> [(Node, Cell)]
listOfStones [] _ = []
listOfStones (((x, y), a) : xs) board
  | isFreedom (x, y) a board = listOfStones xs board
  | otherwise = ((x, y), a) : listOfStones xs board

-- Удаляет камни из списка с доски.
deleteFromBoard :: [(Node, Cell)] -> Board -> Board
deleteFromBoard [] board = board
deleteFromBoard (((x, y), _):xs) board = deleteFromBoard xs (Map.insert (x, y) Empty board )

 --Считает количество убранных камней для обоих игроков.
countStones :: Node -> [(Node, Cell)] -> Node
countStones a [] = a
countStones (x, y) ((_ , a) : xs)
  | a == Cell Black = countStones (x + 1, y) xs
  | otherwise = countStones (x, y + 1) xs

-- Возвращает список соседей для камня, у которых их цвет совпадает с цветом камня
-- neighboursByColor :: Node -> Cell -> Board -> [(Node,Cell)]
-- neighboursByColor  (x,y) cell board = map fix (filter (\(point, a) -> a == (Just cell) ) [((x + 1, y), Map.lookup (x + 1, y) board)
--   , ((x - 1, y), Map.lookup (x - 1, y) board)
--   , ((x, y + 1), Map.lookup (x, y + 1) board)
--   , ((x, y - 1), Map.lookup (x, y - 1) board)
--   ])
--     where
--       fix (point, (Just cell)) = (point, cell)
--  filter (\((k , l), a) -> ((x == k) && ((l == y-1) || (l == y+1))) || ((y == l) && ((l == x-1) || (l == x+1))) && (a == cell)) (Map.toList board)
-- надо проверять только 4 элемента, не весь список


-- Функция которая возвращает False, если у камня и его соседей нет степени свободы,
-- и True в противном случае.
isFreedom :: Node -> Cell -> Board -> Bool
isFreedom _ Empty _ = True
isFreedom _ (CellShadow _) _ = True
isFreedom (x, y) (Cell stone) board
  | (x > screenWidth - 1) || (y > screenHeight - 1) || (x < 0) || (y < 0) = False
  | (Map.lookup (x + 1, y) board) == (Just Empty) ||
    (Map.lookup (x - 1, y) board) == (Just Empty) ||
    (Map.lookup (x, y - 1) board) == (Just Empty) ||
    (Map.lookup (x, y + 1) board) == (Just Empty)
      = True
  |otherwise = (noLastFreeAmount x y board stone [] /= 0)
  -- можно без if
  -- | otherwise = False


-- | удаление мертвых камней при окончании игры
-- Например, когда игра закончилась(оба игрока с пасовали), то остались группы камней,
-- у которых еще есть свобода, но при этом они считаются мертвыми, потому что
-- другой игрок может убрать их за пару ходов. Если у этих камней есть возможность поставить два глаза,
-- то они не могут быть убраны.
--removeDead :: Game -> Game

-- | Поставить камень.
place :: Node -> Stone -> Board -> Board
place p stone = Map.insert p (Cell stone)

-- | Сменить игрока.
switchPlayer :: Stone -> Stone
switchPlayer Black = White
switchPlayer White = Black


-- | Подсчет количество очков
-- самое сложное из всей базовой части это подсчитать очки
-- над этим надо хорошенько подумать.
amountScores :: Board -> Scores
amountScores _ = (0.0, 0.0)

-- | Определить победителя на игровом поле.
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

-- | Радиус картинки камня.
radiusStone :: Float
radiusStone = 0.45

-- | Начальная фора(очки) белого игрока
-- может варьироваться уже по желания, но это позже в индивидуальных частях.
playerComi :: Float
playerComi = 6.5

-- | начальная фора(камни) черного игрока, надо подумать как это реализовать
-- обозначает, сколько камней должен поставит черный игрок перед началом партии
-- это нужно если очковой форы не хватает и игроки слишком разного уровня
-- но не более 9(для доски 19x19), для 9x9 будет не больше 4.
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

-- | Отступы от края экрана.
screenOffset :: Int
screenOffset = 50

-- | Ширина экрана в пикселях.
screenWidth :: Int
screenWidth  = cellSize * boardWidth + screenOffset

-- | Высота экрана в пикселях.
screenHeight :: Int
screenHeight = cellSize * boardHeight + screenOffset
