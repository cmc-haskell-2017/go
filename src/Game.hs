module Game  where
import Data.Monoid()
import Data.Foldable()
import Draw
import Models
import Config

import Graphics.Gloss.Interface.Pure.Game
import Data.Char()
import qualified Data.Map as Map

run :: IO ()
run = do
    play display bgColor fps initGame drawGame handleGame updateGame
  where
    display = InWindow "Game Go" (screenWidth, screenHeight) (200, 200)
    bgColor = makeColorI 245 245 220 255 -- цвет фона, бежевый
    fps     = 60      -- кол-во кадров в секунду

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

-- =========================================
-- Обработка событий
-- =========================================

-- | Обработка событий.
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) Up _ mouse) = placeStone (mouseToCell mouse)
handleGame (EventMotion mouse) = placeShadowStone (mouseToCell mouse)
handleGame (EventKey (SpecialKey KeySpace) Up _ _) = takePass . setPass
handleGame _ = id


-- | Добавление пасса
setPass :: Game -> Game
setPass game
  | (gamePlayer game) == Black = game {numberOfPass = (\(x , y) -> (x+1 , y)) (numberOfPass game)}
  | otherwise = game { numberOfPass = (\(x , y) -> (x , y+1)) (numberOfPass game)}

-- | Обработка пассов
takePass :: Game -> Game
takePass game
  | np == (1,1) = checkGroups game
  | np == (2,2) = gameOver game
  | otherwise = game {gamePlayer = switchPlayer (gamePlayer game)}
  where np = (numberOfPass game)

-- |  Проверка групп
checkGroups :: Game -> Game
checkGroups = id

gameOver :: Game -> Game
gameOver game = game
  { gameWinner = winner game
  , endGame = Just 0.005
  }


-- | Поставить размытый камень под курсором мышки.
placeShadowStone :: Maybe Node -> Game -> Game
placeShadowStone Nothing game = game
placeShadowStone (Just point) game
  | ruleBusy point (gameBoard game) = game
  |otherwise = game {gameBoard = Map.insert point (CellShadow ( gamePlayer game)) (deleteShadows (gameBoard game)) }

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
placeStone :: Maybe Node -> Game -> Game
placeStone Nothing game = game
placeStone (Just point) game =
    case gameWinner game of
      Just _ -> game    -- если есть победитель, то поставить фишку нельзя
      Nothing -> case modifyAt point (gameBoard game) (gamePlayer game) (listBoard game) of --здесь еще нужно дописать функцию преобразования
        Nothing -> game -- если поставить фишку нельзя, ничего не изменится
        Just newBoard -> completeMove (ruleKo (removeStones (changeBoard newBoard game)))

-- | Применяем изменения, которые произошли на доске.
changeBoard :: Board -> Game -> Game
changeBoard board game = game
  { gameBoard  = board
  -- , listBoard = setBoard (gameBoard game) (listBoard game) -- будет ли это работать?
  }

-- | Закончить ход, инициализировать состояние игры для нового хода.
completeMove :: Game -> Game
completeMove game = game
  { gamePlayer = switchPlayer (gamePlayer game)
  , gameScore = amountScores (gameBoard game)
  , listBoard = setBoard (gameBoard game) (listBoard game)
  , numberOfPass = (0, 0)
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
       ammEqBoards (gameBoard onegame) (listBoard onegame) > 1

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

-- | веделить территории на доске для конкретного цвета
-- allocateOfTerritory :: Stone -> Board -> [Board] -- [Board] == [Map Node Cell]
-- allocateOfTerritory stone board =


-- allocateOfBoarder :: Board -> [Board]
-- allocateOfBoarder board

--


-- | Подсчет количество очков
-- самое сложное из всей базовой части это подсчитать очки
-- над этим надо хорошенько подумать.
amountScores :: Board -> Scores
amountScores _ = (0.0, 0.0)

-- | Определить победителя на игровом поле. ничей не должно быть
winner :: Game -> Maybe Stone
winner game
  | (scoreblack game) < (scorewhite game) = Just White
  | otherwise = Just Black
    where
      scoreblack game = fst (gameScore game) + fromIntegral( fst ( scoreStones game))
      scorewhite game = (gameComi game) + snd (gameScore game) + fromIntegral( snd ( scoreStones game))

-- | Обновление игры.
-- В этой игре все изменения происходят только по действиям игрока,
-- поэтому функция обновления — это тождественная функция.
updateGame :: Float -> Game -> Game
updateGame _ = id

-- =========================================
-- Искуственный инелект игры
-- =========================================

-- | Главная функция
ai :: Stone -> Board -> Maybe Move
ai _ _ = Nothing
-- ai stone board = fromBestMove . fold . bestMoves . fmap estimate . cutTree n . gameTree
-- более умная свертка fold

-- minimax :: GameTree (m, e) -> m
-- minimax

-- | Возможные ходы
possibleMoves :: Stone -> Board -> [Move]
possibleMoves _ _ = []

-- | Построение дерева игры
gameTree :: Board -> GameTree b Board
gameTree a = Leaf a

-- | Обрезание дерева игры
cutTree :: Int -> GameTree b a -> GameTree b a
cutTree _ (Leaf a) = Leaf a
-- cutTree n tree@(Node b trees)
--   | n == 0 = Leaf b
--   | otherwise = Node b $ map (\(m, t) -> (m, cutTree (n-1) t)) trees

-- | Оценка игрового поля
estimate :: Board -> Estimate
estimate _ = Estimate 0 0 0.0

-- | Лучшие ходы
bestMoves :: GameTree b Estimate -> GameTree b BestMove
bestMoves (Leaf _) = (Leaf NoMove)
-- bestMoves (Node ts) = Node $ map (\(m, t) -> (m, fmap (BestMove m) t) ) ts
