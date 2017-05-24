module Game  where
import Data.Monoid()
import Data.Foldable()
import Database

import Graphics.Gloss.Interface.Pure.Game
import Data.Map (Map)
import Graphics.Gloss.Interface.IO.Game
-- import Data.Char
import qualified Data.Map as Map
import qualified Data.Text as T
{-}
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow-}


run :: IO ()
run = do
    putStrLn "Input \n1 - for authorization users;\n2 - for registration new user;\n3 - print table of records"
    manage <- getLine
    if (manage == "1")
    then do
      name1 <- authorizationUser
      name2 <- authorizationUser
      playIO display bgColor fps (initGame name1 name2) drawGame handleGame updateGame
    else do
      if (manage == "2")
      then do
        registrationUser
        run
      else do
        printRecord
        run
  where
    display = InWindow "Game Go" (screenWidth, screenHeight) (200, 200)
    bgColor = makeColorI 245 245 220 255 -- цвет фона, бежевый
    fps     = 60      -- кол-во кадров в секунду-}

-- Играем по правилам Американской ассоциации го AGA

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

-- | Кол-во пассов подряд каждого игрока
type Passes = (Int, Int)

-- | Состоние поля, мне кажется оно должно быть таким.
data Game = Game
  { gamePlayer :: Stone -- чей ход
  , gameScore :: Scores -- количества очков для первого и второго игрока
  , gameComi :: Float -- колличество форы
  , gameWinner :: Maybe Stone -- победитель?!
  , gameBoard :: Board
  , listBoard :: [Board] -- список всех предыдущих состояний
  , scoreStones :: AmountStones -- кол-во камней убранных каждым игроком
  , numberOfPass :: Passes
  , nameFirstPlayer :: String
  , nameSecondPlayer :: String
  }

-- | Начальное состояние игры.
-- Игровое поле — пусто.
-- Первый игрок ходит черными.
initGame :: String -> String -> Game
initGame name1 name2 = Game
  { gamePlayer = Black
  , gameScore = (0, 0)
  , gameComi = playerComi
  , gameWinner = Nothing
  , gameBoard  = initBoard
  , listBoard = []
  , scoreStones = (0, 0)
  , numberOfPass = (0, 0)
  , nameFirstPlayer = name1
  , nameSecondPlayer = name2
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
drawGame :: Game -> IO Picture
drawGame game = pure (translate (-w) (-h) (scale c c (pictures
  [ drawGrid
  , drawBoard (gameBoard game)
  -- ,drawPass (numberOfPass game)
  ])))
  where
    c = fromIntegral cellSize
    w = fromIntegral screenWidth  / 2 - offset
    h = fromIntegral screenHeight / 2 - offset
    offset = fromIntegral screenOffset / 2

-- | Отрисовка количества пассов
-- drawPass :: Passes -> Picture
-- drawPass (x,y) = (scale 0.005 0.005 (text [(intToDigit x), (intToDigit y)]))

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

-- =========================================
-- Обработка событий
-- =========================================

-- | Обработка событий.
handleGame :: Event -> Game -> IO Game
handleGame (EventKey (MouseButton LeftButton) Up _ mouse) = placeStone (mouseToCell mouse)
handleGame (EventMotion mouse) = placeShadowStone (mouseToCell mouse)
handleGame (EventKey (SpecialKey KeySpace) Up _ _) = takePass . setPass
handleGame _ = gameIOgame

gameIOgame :: Game -> IO Game
gameIOgame game = pure game

{-
handleGame (EventKey (SpecialKey KeySpace) Up _ _) = pure (takePass . setPass)
handleGame _ = pure id-}


-- | Добавление пасса
setPass :: Game -> Game
setPass game
  | (gamePlayer game) == Black = game {numberOfPass = (\(x , y) -> (x+1 , y)) (numberOfPass game)}
  | otherwise = game { numberOfPass = (\(x , y) -> (x , y+1)) (numberOfPass game)}

-- | Обработка пассов
takePass :: Game -> IO Game
takePass game
  | np == (1,1) = pure (checkGroups game)
  | np == (2,2) = pure (gameOver game)
  | otherwise = pure game
  where np = (numberOfPass game)

-- |  Проверка групп
checkGroups :: Game -> Game
checkGroups = id

gameOver :: Game -> Game
gameOver = id


-- | Поставить размытый камень под курсором мышки.
placeShadowStone :: Maybe Node -> Game -> IO Game
placeShadowStone Nothing game = pure game
placeShadowStone (Just point) game
  | ruleBusy point (gameBoard game) = pure game
  |otherwise = pure game {gameBoard = Map.insert point (CellShadow ( gamePlayer game)) (deleteShadows (gameBoard game)) }

-- | Убрать размытый камень, когда курсора нет.
deleteShadows :: Board -> Board
deleteShadows board =
  (Map.fromList (map (\(p, a) -> if ((a == (CellShadow Black)) ||
  (a == (CellShadow White))) then (p, Empty) else (p, a)) (Map.toList board)))

-- | Получить координаты клетки под мышкой(потом будет распознование близости мышки с пересечением сетки).
mouseToCell :: Point -> Maybe Node
mouseToCell (x, y) = (Just (i, j))
  where
    i = floor (x + fromIntegral screenWidth  / 2) `div` cellSize
    j = floor (y + fromIntegral screenHeight / 2) `div` cellSize

-- | Поставить камень и сменить игрока (если возможно).
placeStone :: Maybe Node -> Game -> IO Game
placeStone Nothing game = pure game
placeStone (Just point) game =
    case gameWinner game of
      Just _ -> pure game    -- если есть победитель, то поставить фишку нельзя
      Nothing -> case modifyAt point (gameBoard game) (gamePlayer game) (listBoard game) of --здесь еще нужно дописать функцию преобразования
        Nothing -> pure game -- если поставить фишку нельзя, ничего не изменится
        Just newBoard -> completeMove (ruleKo (removeStones (changeBoard newBoard game)))

-- | Применяем изменения, которые произошли на доске.
changeBoard :: Board -> Game -> Game
changeBoard board game = game
  { gameBoard  = board
  -- , listBoard = setBoard (gameBoard game) (listBoard game) -- будет ли это работать?
  }

-- | Закончить ход, инициализировать состояние игры для нового хода.
completeMove :: Game -> IO Game
completeMove game = do
  updateTableRec (nameFirstPlayer(game), nameSecondPlayer(game), fst(amountScores (gameBoard(game))), snd(amountScores(gameBoard(game))))
  pure game
    { gamePlayer = switchPlayer (gamePlayer game)
    , gameScore = amountScores (gameBoard game)
    , gameWinner = winner game
    , listBoard = setBoard (gameBoard game) (listBoard game)
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
isPossible point board stone _
  | ruleBusy point board = False
  -- | (not (ruleKo point stone board listboard)) = False
  | (not (ruleFreedom point board stone)) = False
  | otherwise = True


ruleKo :: Game -> Game
ruleKo game
  | func game = game
    { gameBoard = head (listBoard game)
    , listBoard = tail (listBoard game)
    , gamePlayer = switchPlayer (gamePlayer game)
    -- , scoreStones надо будет отменять изменения обратно
    }
  | otherwise = game
    where
      func onegame = (listBoard onegame) /= [] &&
        tail (listBoard onegame) /= [] &&
       (gameBoard onegame) == head (tail (listBoard onegame)) ||
       ammEqBoards (gameBoard onegame) (listBoard onegame) >= 3

-- | Правило Ко борьбы, true если все по правилам.
-- Конкретно данное состояние встретилось менее трех раз и оно не совпало с предыдущим => все норм
-- ruleKo :: Node -> Stone -> Board -> [Board] -> Bool
-- ruleKo _ _ _ [] = True
-- ruleKo point stone board (x:xs) =
--   place point stone board /= x && ammEqBoards board (x:xs) < 3

-- | Сколько раз встречалась такая доска раньше.
ammEqBoards :: Board -> [Board] -> Int
ammEqBoards board = length . filter (== board)

-- | Правило свободы.
-- | Возвращает True, если все ок (можно ставить камень).
ruleFreedom :: Node -> Board -> Stone -> Bool
ruleFreedom (point_row, point_col) board stone =
  cmpFieldWithEmpty (point_row - 1) point_col board stone ||
  cmpFieldWithEmpty (point_row + 1) point_col board stone ||
  cmpFieldWithEmpty point_row (point_col - 1) board stone ||
  cmpFieldWithEmpty point_row (point_col + 1) board stone

-- | Возвращает True если правило свободы выполненно для данного соседа.
cmpFieldWithEmpty :: Int -> Int -> Board -> Stone -> Bool
cmpFieldWithEmpty point_row point_col board stone
  | borderCmp point_row point_col = False
  | (Map.lookup (point_row, point_col) board) == (Just (Cell (switchPlayer stone))) &&
                                                 (amountOf (switchPlayer stone) == 0)
                                                  = True
  | otherwise = (Map.lookup (point_row, point_col) board) == (Just Empty) ||
              (amountOf stone >= 1)
    where
      amountOf stone2 = amountOfFreedom point_row point_col board stone2 -- Переименовать эту функцию

-- | Кол-во свободы у ближайщих групп.
amountOfFreedom :: Int -> Int -> Board -> Stone -> Int
amountOfFreedom point_row point_col board stone =
  length (delSimil (noLastFree point_row point_col board stone []))

-- | Удаление одинаковых элемнтов списка.
delSimil :: [(Int, Int)] -> [(Int, Int)]
delSimil [] = []
delSimil ((x, y):xs) = (x, y):delSimil (filter (\(x2, y2) -> x2 /= x && y2 /= y) xs)

-- | Возвращает список из координат свободных позиций вокруг группы камней (с повторениями)
noLastFree :: Int -> Int -> Board -> Stone -> [(Int, Int)] -> [(Int, Int)]
noLastFree row col board stone list
  | borderCmp row col || ((filter (\(x, y) -> x == row && y == col) list) /= []) = []
  | (Map.lookup (row, col) board) == (Just Empty) = [(row, col)]
  | (Map.lookup (row, col) board) == (Just (Cell stone)) =
    noLastFree (row - 1) col board stone ((row, col) : list) ++
    noLastFree (row + 1) col board stone ((row, col) : list) ++
    noLastFree row (col - 1) board stone ((row, col) : list) ++
    noLastFree row (col + 1) board stone ((row, col) : list)
  | otherwise = []

-- | Возвращает True если вышли за границы поля.
borderCmp :: Int -> Int -> Bool
borderCmp point_row point_col = point_row < 0 ||
                                point_row > boardHeight - 1 ||
                                point_col < 0 ||
                                point_col > boardWidth - 1

-- | Занято ли место, false если не занято.
ruleBusy :: Node -> Board -> Bool
ruleBusy p board
  | Map.lookup p board == (Just Empty) || Map.lookup p board == (Just (CellShadow Black)) || Map.lookup p board == (Just (CellShadow White)) = False
  | otherwise = True

-- | Убрать камни без свободы и засчитать другому игроку столько очков,
-- сколько было убрано камней.
removeStones :: Game -> Game
removeStones game = game
  { gameBoard  = (deleteFromBoard loS (gameBoard game))
  , scoreStones = countStones (scoreStones game)  loS
  }
    where
      loS = listOfStones (Map.toList (gameBoard game)) (gameBoard game) (gamePlayer game)

-- | Возвращает список камней которые, необходимо удалить.
listOfStones :: [(Node, Cell)] -> Board -> Stone -> [(Node, Cell)]
listOfStones [] _ _ = []
listOfStones (((x, y), a) : xs) board stone
  | a == (Cell stone) || isFreedom (x, y) a board = listOfStones xs board stone
  | otherwise = ((x, y), a) : listOfStones xs board stone

-- | Удаляет камни из списка с доски.
deleteFromBoard :: [(Node, Cell)] -> Board -> Board
deleteFromBoard [] board = board
deleteFromBoard (((x, y), _):xs) board = deleteFromBoard xs (Map.insert (x, y) Empty board )

 --Считает количество убранных камней для обоих игроков.
countStones :: Node -> [(Node, Cell)] -> Node
countStones a [] = a
countStones (x, y) ((_ , a) : xs)
  | a == Cell Black = countStones (x + 1, y) xs
  | otherwise = countStones (x, y + 1) xs

-- | Функция которая возвращает False, если у камня и его соседей нет степени свободы,
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
  |otherwise = (amountOfFreedom x y board stone /= 0)


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
amountScores :: Board -> (Float, Float)
amountScores board = (amountBlackStone board, amountWhiteStone board) --(20000.0, 40000)

amountBlackStone :: Board -> Float
amountBlackStone board = floatFromInt(length(filter (\(x,y) -> y == (Cell Black)) (Map.toList board)))

amountWhiteStone :: Board -> Float
amountWhiteStone board = floatFromInt(length(filter (\(x,y) -> y == (Cell White)) (Map.toList board)))

floatFromInt :: Int -> Float
floatFromInt x = fromIntegral x

-- | Определить победителя на игровом поле.
winner :: Game -> Maybe Stone
winner _ = Nothing

-- | Обновление игры.
-- В этой игре все изменения происходят только по действиям игрока,
-- поэтому функция обновления — это тождественная функция.
updateGame :: Float -> Game -> IO Game
updateGame _ = gameIOgame

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
