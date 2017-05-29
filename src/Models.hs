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

-- | Цвет, которым играет ИИ
type AIColor = Stone

-- | Лучший ход
data BestMove
  = NoMove
  | BestMove Move Estimate

-- | Дерево игры
-- data GameTree b a = Leaf a | Node b [(Move, GameTree b a)]
data GameTree a = Leaf a | Node a [(Move, GameTree a)]
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
  , gameComi :: Float -- колличество форы для белых камней
  , gameWinner :: Maybe Stone -- победитель
  , gameBoard :: Board
  , listBoard :: [Board] -- список всех предыдущих состояний
  , scoreStones :: AmountStones -- кол-во камней убранных каждым игроком
  , numberOfPass :: Passes
  , endGame :: Maybe Float
  , typeAI :: AIColor
  , movePlayer :: Bool
  , sizecutTree :: Int
  }

-- | Имя игрока
type Name = String

-- | Пароль
type Password = String

-- | Информация о игроке
type User = (Name, Password)

-- | Список игроков
type Users = [User]

-- | Тип игры - ИИ, 2 игрока
data GameType = AIGame | PlayersGame
  deriving(Eq, Show)

data Screen =
    ScreenLogin LoginScreen                -- Первый экран - ввод логина и пароля
  | ScreenMainMenu MainMenuScreen          -- Главное меню
  | ScreenGame Game                        -- Игра
  | ScreenRegistration RegistrationScreen  -- Окно регистрации
  | ScreenRecords RecordsScreen            -- Окно со списком рекордов
  | ScreenSettings SettingsScreen          -- Окно настроек игры

-- | Экран ввода логина и пароля
data LoginScreen = LoginScreen
  { loginFocusButton :: Maybe LoginButton  -- Клавиша, на которую наведен курсор
  , loginState :: Maybe WriteState         -- Нажата ли кнопка ввода логина или пароля
  , name :: Name                           -- Введенное имя
  , password :: Password                   -- Введенный пароль
  }

-- | Клавиши на экране ввода логина и пароля
data LoginButton = Login | Password | Registration | Enter
  deriving(Eq, Show)

-- | Состояния ввода
data WriteState = LoginState | PasswordState
  deriving(Eq, Show)

-- | Экран главного меню
data MainMenuScreen = MainMenuScreen
  { menuFocusButton :: Maybe MenuButton  -- Клавиша, на которую наведен курсор
  , gameType :: GameType                 -- Тип игры
  }

-- | Клавиши на экране главного меню
data MenuButton = NewGame | Records | Settings
  deriving(Eq, Show)

-- | Экран регистрации
data RegistrationScreen = RegistrationScreen
  { registrationButton :: Maybe RegistrationButton  -- Клавиша, на которую наведен курсор
  , registrationState :: Maybe WriteState           -- Нажата ли кнопка ввода логина и пароля
  , newName :: Name                                 -- Регистрируемое имя
  , newPassword :: Password                         -- Пароль для нового имени
  }

-- | Кнопки на экране регистрации
data RegistrationButton =
    EnterRegistration      -- Подтверждение нового имени и пароля
  | ExitRegistration       -- Отмена регистрации - выход на экран ввода логина и пароля
  | LoginRegistration      -- Ввод логина
  | PasswordRegistration   -- Ввод пароля
  deriving(Eq, Show)

-- | Экран рекордов
data RecordsScreen = RecordsScreen
  { recordsButton :: Maybe RecordsButton  -- Клавиша, на которую наведен курсор
  , recordsGameType :: GameType           -- Тип игры
  }

-- | Кнопка на экране рекордов
data RecordsButton = ExitRecords
  deriving(Eq, Show)

-- | Экран настроек
data SettingsScreen = SettingsScreen
  { settingsButton :: Maybe SettingsButton   -- Клавиша, на которую наведен курсор
  , settingsGameType :: GameType             -- Тип игры
  }

-- | Кнопки на экране настроек
data SettingsButton = ExitSettings | AIGameType | PlayersGameType
  deriving(Eq, Show)

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
  , endGame = Nothing
  , typeAI = defaultAIColor
  , movePlayer = False
  , sizecutTree = defaultsizecut
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

-- | Начальное состояние экрана - экран ввода логина и пароля
initScreen :: Screen
initScreen = ScreenLogin LoginScreen
  { loginFocusButton = Nothing        -- Кнопка никакая еще не выделена
  , loginState = Nothing              -- Не было входа в состояния ввода
  , name = ""                         -- Имя еще не введено
  , password = ""                     -- Пароль еще не введен
  }

-- | ИИ по умолчанию белые
defaultAIColor :: AIColor
defaultAIColor = White

-- | размер обрезки дерева
defaultsizecut :: Int
defaultsizecut = 3
