{-# LANGUAGE DeriveFunctor #-}
module Models where

import qualified Data.Map as Map
import Config

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
type Board = Map.Map Node Cell

-- | Кол-во камней, которые "съели" черные.
type Blacksum = Int

-- | Кол-во камней, которые "съели" белые.
type Whitesum = Int

-- | Кол-во камней убранных каждым игроком.
type AmountStones = (Blacksum, Whitesum)

-- | Кол-во пассов подряд каждого игрока
type Passes = (Int, Int)

-- | Ход игрока
type Move = Node

-- | Лучший ход
data BestMove
  = NoMove
  | BestMove Move Estimate

-- | Дерево игры
data GameTree b a = Leaf a | Node b [(Move, GameTree b a)]
-- data GameTree a = Leaf a | Node [(Move, GameTree a)]
  deriving(Functor)

-- GameTree a () -> GameTree () a

-- | Оценка поля
data Estimate = Estimate Score Int Float
  deriving (Eq, Ord)

-- | Моноид для лучшего хода
instance Monoid BestMove where
  mempty = NoMove
  mappend (BestMove m1 e1) (BestMove m2 e2)
    | e1 >= e2  = BestMove m1 e1
    | otherwise = BestMove m2 e2
  mappend NoMove bm = bm
  mappend bm NoMove = bm

-- | Состоние поля, мне кажется оно должно быть таким.
data Game = Game
  { gamePlayer :: Stone -- чей ход
  , gameScore :: Scores -- количества очков для первого и второго игрока
  , gameComi :: Float -- колличество форы
  , gameWinner :: Maybe Stone -- победитель
  , gameBoard :: Board
  , listBoard :: [Board] -- список всех предыдущих состояний
  , scoreStones :: AmountStones -- кол-во камней убранных каждым игроком
  , numberOfPass :: Passes
  }

-- =========================================
-- Инициализация игры
-- =========================================

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
  , numberOfPass = (0, 0)
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
