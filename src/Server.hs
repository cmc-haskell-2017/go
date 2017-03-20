module Server  where

run :: IO ()
run = putStrLn "This project is not yet implemented!!!"

-- =========================================
-- Модель игры
-- =========================================

-- Модель камня игрока 
data Stone = Black | White
  deriving(Eq, Show)

-- Клетка поля, но это не точно
data Cell = Maybe Stone 

-- Количество очков для одного игрока
type Score = Float

-- Количество очков двух игроков
type Scores = (Score, Score)

-- Над игровым полем еще надо подумать
-- data Board

-- состоние поля, мне кажется оно должно быть таким 
data Game =
  { gamePlayer :: Stone
  , gameScore :: Scores -- количества очков для первого и второго игрока
  , gameComi :: Float -- колличество форы
  , gameWinner :: Maybe Stone -- победитель?!
    --GameBoard :: Board
  }


-- =========================================
-- Обработка событий
-- =========================================

-- Обработка событий.
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) _ _ mouse) = placeStone (mouseToCell mouse)
handleGame _ = id


-- Поставить камень и сменить игрока (если возможно).
placeStone :: (Int, Int) -> Game -> Game

-- Проверка на правила игры


-- Подсчет количество очков
Score::Board -> Game -> Scores

-- =========================================
-- Константы, параметры игры
-- =========================================


