module Game  where
import Data.Monoid()
import Data.Foldable(fold)
import Draw
import Models
import Config


import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map as Map

run :: IO()
run = do
    play display bgColor fps initScreen drawScreen handleScreen updateScreen
  where
    display = InWindow "Game Go" (screenWidth, screenHeight) (200, 200)
    bgColor = makeColorI 245 245 220 255 -- цвет фона, бежевый
    fps     = 60      -- кол-во кадров в секунду

-- | Обработка события на экране ввода логина и пароля
handleLoginScreen :: Event -> Screen -> Screen
handleLoginScreen (EventKey (MouseButton LeftButton) _ _ mouse) screen
  = changeLoginScreen mouse screen
handleLoginScreen (EventMotion mouse) screen = checkScreen mouse screen
handleLoginScreen (EventKey (Char c) Down _ _ ) screen = readStr c screen
handleLoginScreen (EventKey (SpecialKey KeyDelete) _ _ _ ) screen
  = deleteKey screen
handleLoginScreen _ screen = screen

-- | Обработка события на экране главного меню
handleMainMenuScreen :: Event -> Screen -> Screen
handleMainMenuScreen (EventKey (MouseButton LeftButton) Up _ _) screen
  = changeMainMenuScreen screen
handleMainMenuScreen (EventMotion mouse) screen = checkScreen mouse screen
handleMainMenuScreen _ screen = screen

-- | Обработка события на экране регистрации
handleRegistrationScreen :: Event -> Screen -> Screen
handleRegistrationScreen (EventKey (MouseButton LeftButton) _ _ mouse) screen
  = changeRegistrationScreen mouse screen
handleRegistrationScreen (EventMotion mouse) screen = checkScreen mouse screen
handleRegistrationScreen (EventKey (Char c) Down _ _ ) screen = readStr c screen
handleRegistrationScreen (EventKey (SpecialKey KeyDelete) _ _ _ ) screen
  = deleteKey screen
handleRegistrationScreen _ screen = screen

-- | Обработка события на экране рекордов
handleRecordsScreen :: Event -> Screen -> Screen
handleRecordsScreen (EventKey (MouseButton LeftButton) _ _ _) screen
  = changeRecordsScreen screen
handleRecordsScreen (EventMotion mouse) screen = checkScreen mouse screen
handleRecordsScreen _ screen = screen

-- | Обработка события на экране настроек
handleSettingsScreen :: Event -> Screen -> Screen
handleSettingsScreen (EventKey (MouseButton LeftButton) _ _ _) screen
  = changeSettingsScreen screen
handleSettingsScreen (EventMotion mouse) screen = checkScreen mouse screen
handleSettingsScreen _ screen = screen


-- | Обработка события
handleScreen :: Event -> Screen -> Screen
handleScreen (EventKey (Char _) Down _ _ ) (ScreenGame _)
  = ScreenMainMenu MainMenuScreen
    { menuFocusButton = Nothing
    , gameType = PlayersGame
    }
handleScreen event (ScreenGame game)
  = ScreenGame (handleGame event game)
handleScreen event (ScreenLogin loginScreen)
  = handleLoginScreen event (ScreenLogin LoginScreen
    { loginFocusButton = (loginFocusButton loginScreen)
    , loginState = (loginState loginScreen)
    , name = (name loginScreen)
    , password = (password loginScreen)
    })
handleScreen event (ScreenMainMenu menuScreen)
  = handleMainMenuScreen event (ScreenMainMenu MainMenuScreen
    { menuFocusButton = (menuFocusButton menuScreen)
    , gameType = (gameType menuScreen)
    })
handleScreen event (ScreenRecords recordsScreen)
  = handleRecordsScreen event (ScreenRecords RecordsScreen
    { recordsButton = (recordsButton recordsScreen)
    , recordsGameType = (recordsGameType recordsScreen)
     })
handleScreen event (ScreenSettings settingsScreen)
  = handleSettingsScreen event (ScreenSettings SettingsScreen
    { settingsButton = (settingsButton settingsScreen)
    , settingsGameType = (settingsGameType settingsScreen)
    })
handleScreen event (ScreenRegistration regScreen)
  = handleRegistrationScreen event (ScreenRegistration RegistrationScreen
    { registrationButton = (registrationButton regScreen)
    , registrationState = (registrationState regScreen)
    , newName = (newName regScreen)
    , newPassword = (newPassword regScreen)
    })
--handleScreen _ screen = screen

-- | Нажат delete - удалить последней символ
deleteKey :: Screen -> Screen
deleteKey (ScreenLogin screen)
  | (loginState screen) == (Just LoginState)
    = ScreenLogin LoginScreen
      { loginFocusButton = (loginFocusButton screen)
      , loginState = (loginState screen)
      , name = deleteLastChar (name screen)
      , password = (password screen)
      }
  | (loginState screen) == (Just PasswordState)
    = ScreenLogin LoginScreen
      { loginFocusButton = (loginFocusButton screen)
      , loginState = (loginState screen)
      , name = (name screen)
      , password = deleteLastChar (password screen)
      }
deleteKey (ScreenRegistration regScreen)
  | (registrationState regScreen) == (Just LoginState)
    = ScreenRegistration RegistrationScreen
      { registrationButton = (registrationButton regScreen)
      , registrationState = (registrationState regScreen)
      , newName = deleteLastChar (newName regScreen)
      , newPassword = (newPassword regScreen)
      }
  | (registrationState regScreen) == (Just PasswordState)
    = ScreenRegistration RegistrationScreen
      { registrationButton = (registrationButton regScreen)
      , registrationState = (registrationState regScreen)
      , newName = (newName regScreen)
      , newPassword = deleteLastChar (newPassword regScreen)
      }
deleteKey screen = screen

-- | Удаление последнего символа данной строки
deleteLastChar :: String -> String
deleteLastChar "" = ""
deleteLastChar (_ : []) = []
deleteLastChar (x : xs) = x : deleteLastChar xs

-- | Считываем и записываем символ
readStr :: Char -> Screen -> Screen
readStr c (ScreenLogin screen)
  | (loginState screen) == (Just LoginState)
    = ScreenLogin LoginScreen
      { loginFocusButton = (loginFocusButton screen)
      , loginState = (loginState screen)
      , name = (name screen) ++ (c : [])
      , password = (password screen)
      }
  | (loginState screen) == (Just PasswordState)
    = ScreenLogin LoginScreen
      { loginFocusButton = (loginFocusButton screen)
      , loginState = (loginState screen)
      , name = (name screen)
      , password = (password screen) ++ (c : [])
      }
readStr c (ScreenRegistration regScreen)
  | (registrationState regScreen) == (Just LoginState)
    = ScreenRegistration RegistrationScreen
      { registrationButton = (registrationButton regScreen)
      , registrationState = (registrationState regScreen)
      , newName = (newName regScreen) ++ (c : [])
      , newPassword = (newPassword regScreen)
      }
  | (registrationState regScreen) == (Just PasswordState)
    = ScreenRegistration RegistrationScreen
      { registrationButton = (registrationButton regScreen)
      , registrationState = (registrationState regScreen)
      , newName = (newName regScreen)
      , newPassword = (newPassword regScreen) ++ (c : [])
      }
readStr _ screen = screen

updateScreen :: Float -> Screen -> Screen
updateScreen _ = id


------------------------------------------------------------------------------------------


---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------

-- Играем по правилам Американской ассоциации го AGA

-- памятка
--play :: Display
  -- -> Color                                  bgColor
  -- -> Int                                    fps
  -- -> world -- world - переменная типа       initGame
  -- -> (world -> Picture) -- view             draw
  -- -> (Event -> world -> world) - обработка событий(нажатие на кнопку мышки)
  -- -> (Float -> world -> world) - обновление для каждого кадра
  -- -> IO ()

-- =========================================
-- Обработка событий игры
-- =========================================

-- | Обработка событий.
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) Up _ mouse) = moveAI . placeStone (mouseToCell mouse)
handleGame (EventMotion mouse) = placeShadowStone (mouseToCell mouse)
handleGame (EventKey (SpecialKey KeySpace) Up _ _) = passAI . takePass . setPass
handleGame _ = id

-- | пасс ИИ
passAI :: Game -> Game
passAI game
  | checkScore (gameScore game) (typeAI game) = takePass $ setPass game
  | otherwise = moveAI $ game {numberOfPass = (0, 0), movePlayer = True}
    where
      checkScore (x, y) stoneai
        | stoneai == Black = x >= y
        | otherwise = x <= y

-- | Добавление пасса
setPass :: Game -> Game
setPass game
  | (gamePlayer game) == Black = game {numberOfPass = (\(x , y) -> (x + 1 , y)) (numberOfPass game)}
  | otherwise = game { numberOfPass = (\(x , y) -> (x , y + 1)) (numberOfPass game)}

-- | Обработка пассов
takePass :: Game -> Game
takePass game
  | np == (1,1) = (checkGroups game) {gamePlayer = switchPlayer (gamePlayer game)}
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
        Just newBoard -> completeMove (removeStones (changeBoard newBoard game))
        -- Just newBoard -> completeMove (ruleKo (removeStones (changeBoard newBoard game)))

-- | Применяем изменения, которые произошли на доске.
changeBoard :: Board -> Game -> Game
changeBoard board game = game
  { gameBoard  = board
  -- , listBoard = setBoard (gameBoard game) (listBoard game) -- будет ли это работ  ать?
  }

-- | Закончить ход, инициализировать состояние игры для нового хода.
completeMove :: Game -> Game
completeMove game = game
  { gamePlayer = switchPlayer (gamePlayer game)
  , gameScore = amountScores (gameBoard game) + float (scoreStones game)
  , listBoard = setBoard (gameBoard game) (listBoard game)
  , numberOfPass = (0, 0)
  , movePlayer = True
  }
    where
      float (x, y) = (fromIntegral y, fromIntegral x) -- обратный порядок потому что перепутан подсчет сьеденных камней

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
  | (not (ruleFreedom point board stone)) = False
  | otherwise = True

-- | Проверка на правила игры, false если не по правилам.
-- isPossible :: Node -> Board -> Stone -> [Board] -> Bool
-- isPossible point board stone _
--   | ruleBusy point board = False
--   -- | (not (ruleKo point stone board listboard)) = False
--   | (not (ruleFreedom point board stone)) = False
--   | otherwise = True

-- ruleKo :: Game -> Game
-- ruleKo game
--   | func game = game
--     { gameBoard = head (listBoard game)
--     , listBoard = tail (listBoard game)
--     , gamePlayer = switchPlayer (gamePlayer game)
--     -- , scoreStones надо будет отменять изменения обратно
--     }
--   | otherwise = game
--     where
--       func onegame = (listBoard onegame) /= [] &&
--         tail (listBoard onegame) /= [] &&
--        (gameBoard onegame) == head (tail (listBoard onegame)) ||
--        ammEqBoards (gameBoard onegame) (listBoard onegame) > 1

-- | Правило Ко борьбы, true если все по правилам.
-- Конкретно данное состояние встретилось менее трех раз и оно не совпало с предыдущим => все норм
ruleKo :: Node -> Stone -> Board -> [Board] -> Bool
ruleKo _ _ _ [] = True
ruleKo _ _ _ [x] = True
ruleKo point stone board (x:y:xs)
--  = amountEqBoards board (x:xs) < 1
--  && boardTry (place point stone board) (x:xs) /= x
  = boardTry stone (place point stone board) (x:y:xs) /= y
--
-- | Создание доски для проверки на равенство
boardTry :: Stone -> Board -> [Board] -> Board
boardTry stone board boards = (gameBoard (removeStones (fakeGame stone board boards)))

-- | Game для удаления камня
fakeGame :: Stone -> Board -> [Board] -> Game
fakeGame stone board boards = Game
  { gamePlayer = stone
  , gameScore = (0, 0)
  , gameComi = playerComi
  , gameWinner = Nothing
  , gameBoard  = board
  , listBoard = boards
  , scoreStones = (0, 0)
  , numberOfPass = (0, 0)
  , endGame = Nothing
  , typeAI = defaultAIColor
  , movePlayer = False
  , sizecutTree = 0
  }

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

-- | Почти две одинаковые функции, это не порядок.
-- noLastFreeAmount :: Int -> Int -> Board -> Stone -> [(Int, Int)] -> Int
-- noLastFreeAmount row col board stone list
--   | borderCmp row col = 0
--   | (filter (\(x, y) -> x == row && y == col) list) /= [] = 0
--   | (Map.lookup (row, col) board) == (Just Empty) = 1
--   | (Map.lookup (row, col) board) == (Just (Cell stone)) = noLastFreeAmount (row - 1) col board stone ((row, col) : list) +
--                                                            noLastFreeAmount (row + 1) col board stone ((row, col) : list) +
--                                                            noLastFreeAmount row (col - 1) board stone ((row, col) : list) +
--                                                            noLastFreeAmount row (col + 1) board stone ((row, col) : list)
  -- | otherwise = 0

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
countStones :: AmountStones -> [(Node, Cell)] -> Node
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
place :: Move -> Stone -> Board -> Board
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

-- | Подсчет количество очков
-- самое сложное из всей базовой части это подсчитать очки
-- над этим надо хорошенько подумать.
amountScores :: Board -> Scores
amountScores board = (stoneScore board Black, stoneScore board White)

stoneScore :: Board -> Stone -> Float
stoneScore board stone = fromIntegral $ Map.size $ Map.filter (== (Cell stone)) board


-- | Определить победителя на игровом поле. ничей не должно быть
winner :: Game -> Maybe Stone
winner game
  | (scoreblack game) < (scorewhite game) = Just White
  | otherwise = Just Black
    where
      scoreblack act = fst (gameScore act) + fromIntegral( fst ( scoreStones act))
      scorewhite act = (gameComi act) + snd (gameScore act) + fromIntegral( snd ( scoreStones act))

-- | Обновление игры.
-- В этой игре все изменения происходят только по действиям игрока,
-- поэтому функция обновления — это тождественная функция.
updateGame :: Float -> Game -> Game
updateGame _ = id


-- =========================================
-- Искуственный инелект игры
-- =========================================

-- ход, который делает ИИ, если игрок сделал свой ход
moveAI :: Game -> Game
moveAI game =
  case (movePlayer game) of
    False -> game
    True -> case ai (typeAI game) (gameBoard game) (sizecutTree game) (listBoard game) (scoreStones game)  of
      Nothing -> game {gamePlayer = switchPlayer (gamePlayer game), numberOfPass = setAIpass game, movePlayer = False}
      (Just move) -> complateAImove move (typeAI game) game
      where
        setAIpass gamepass
          | typeAI gamepass == White = setpass (numberOfPass gamepass) White
          | otherwise = setpass (numberOfPass gamepass) Black
        setpass (b, w) White = (b, w + 1)
        setpass (b, w) Black = (b + 1, w)

-- | закончить ход за ИИ
complateAImove :: Move -> Stone -> Game -> Game
complateAImove move stone game = aicomplate $ completeMove (removeStones (changeBoard (place move stone (gameBoard game)) game))
  where
    aicomplate g = g {movePlayer = False, numberOfPass = (0, 0)}

-- | Главная функция
ai :: AIColor -> Board -> Int -> [Board] -> AmountStones -> Maybe Move
ai stone board n boards amountStones =
  case maxmin stone (cutTree n $ gameTree stone board boards amountStones) Nothing of
    NoMove -> Nothing
    BestMove move _ -> (Just move)

-- | Построение дерева игры
gameTree :: Stone -> Board -> [Board] -> AmountStones -> GameTree AmountStones Board
gameTree stone board boards amountStones =
  Node amountStones board $ map (\m -> (m, gameTree (switchPlayer stone) (repairBoard m stone board) (board : boards) $ scoreStone stone (place m stone board) ) ) $ possibleMoves stone board boards
  where
    repairBoard node stoneinter boardinter = removeStone (place node stoneinter boardinter) stoneinter
    scoreStone s b  = countStones amountStones $ listOfStones (Map.toList b) b s
    removeStone b s = (deleteFromBoard loS b)
      where
        loS = listOfStones (Map.toList b) b s

-- | Обрезание дерева игры
cutTree :: Int -> GameTree b a -> GameTree b a
cutTree _ (Leaf b a) = Leaf b a
cutTree n (Node b a trees)
  | n == 0 = Leaf b a
  | otherwise = Node b a $ map (\(m, t) -> (m, cutTree (n - 1) t)) trees

-- ai stone board = fromBestMove . fold . bestMoves . fmap estimate . cutTree n . gameTree
-- ai stone board =  (minmax stone  cutTree 3 . gameTree
-- более умная свертка fold

-- alpha -> beta -> color -> Board
-- alphabeta :: Estimate -> Estimate -> Stone -> GameTree (m, e) -> m
-- alphabeta :: Estimate -> Estimate -> Stone -> GameTree Board -> BestMove
-- alphabeta alpha beta stone gametree
--   | isTerminal gametree = minus (heuristic gametree stone)
--   | otherwise = do
--     bestmove <- (BestMove (0, 0) beta)
--     bestmove <- fmap f
--     return bestmove

-- | Алгоритм минимакса
maxmin :: Stone -> GameTree AmountStones Board -> Maybe Move -> BestMove
maxmin _ (Leaf _ _) Nothing = NoMove
maxmin stone gametree move
  | isTerminal gametree = BestMove (getmove move) (heuristic gametree stone)
  | otherwise = fold $ map (\(m, t) -> makeBestMove m $ maxmin (switchPlayer stone) t (Just m)) (childs gametree)
  where
    getmove (Just m) = m
    getmove Nothing = (0, 0) -- допилить/ можно поставить сюда все, что угодно. Сюда никогда не зайдет, а можно ли тогда убрать это?
    makeBestMove _ NoMove = NoMove -- -||- никогда сюда не зайдет
    makeBestMove pastMove (BestMove _ e) = BestMove pastMove e

--
-- min :: Stone -> GameTree Board -> BestMove
-- f - s = -AlphaBeta(child, -score, -alpha, deph+1, -player)
-- делаем из GameTree board GameTree Estimate
-- filter -> fold
-- if s < score  then score  = s
-- if score <= alpha then return score
-- альфа-бета пока не получается


childs :: GameTree b a -> [(Move, GameTree b a)]
childs (Leaf _ _) = []
childs (Node _ _ trees) = trees

-- | Возможные ходы
possibleMoves :: Stone -> Board -> [Board] -> [Move]
possibleMoves stone board boards =
  map (\(k, _) -> k) $ cutBoard board $ Map.filterWithKey (\k _ -> isPossible k board stone boards) board

-- | Уменьшение доски, чтобы рассматривать меньше вариантов
cutBoard :: Board -> Board -> [(Node, Cell)]
cutBoard board1 board2 =
   regionStone board2 $ Map.toList $ Map.filter ( /= Empty) board1

--
regionStone :: Board -> [(Node, Cell)]-> [(Node, Cell)]
regionStone _ [] = []
regionStone fullBoard (x : xs) = (regionStoneWithRadius fullBoard x radiuscut) ++ regionStone fullBoard xs

--
regionStoneWithRadius :: Board -> (Node, Cell) -> Int -> [(Node, Cell)]
regionStoneWithRadius board ((x, y), _) n =
  (addCell (x + n, y) $ Map.lookup (x + n, y) board) ++
  (addCell (x + n, y - n) $ Map.lookup (x + n, y - n) board) ++
  (addCell (x, y - n) $ Map.lookup (x, y - n) board) ++
  (addCell (x - n, y - n) $ Map.lookup (x - n, y - n) board) ++
  (addCell (x - n, y) $ Map.lookup (x - n, y) board) ++
  (addCell (x - n, y + n) $ Map.lookup (x - n, y + n) board) ++
  (addCell (x, y + n) $ Map.lookup (x, y + n) board) ++
  (addCell (x + n, y + n) $ Map.lookup (x + n, y + n) board)
  where
    addCell _ Nothing = []
    addCell node (Just a) = [(node, a)]


-- | Проверка на конечное состояние
isTerminal :: GameTree b a -> Bool
isTerminal  (Leaf _ _) = True
isTerminal _ = False

-- | Эврестическая оценка поля
heuristic :: GameTree AmountStones Board -> Stone -> Estimate
heuristic (Node _ _ _) _ = Estimate 0 0 0 0.0
heuristic (Leaf amountStones board) stone = Estimate 0 (stones amountStones stone) (Map.size $ Map.filter (== (Cell stone)) board) 0.0
  where
    stones (w, b) s
      | s == Black = b
      | otherwise = w
-- minus :: BestMove -> BestMove
-- minus = id

-- | Оценка игрового поля
-- estimate :: GameTree Board-> Stone -> Estimate
-- estimate _ _ = Estimate 0 0 0.0

-- | Лучшие ходы
-- bestMoves :: GameTree b Estimate -> GameTree b BestMove
-- bestMoves :: GameTree Estimate -> GameTree BestMove
-- bestMoves (Leaf _) = (Leaf NoMove)
-- bestMoves (Node ts) = Node $ map (\(m, t) -> (m, fmap (BestMove m) t) ) ts

--
min_value :: Estimate
min_value = Estimate 0 0 0 0.0

max_value :: Estimate
max_value = Estimate 0 0 0 0.0

radiuscut :: Int
radiuscut
  | sizeBoard == 9 = 1
  | sizeBoard == 13 = 2
  | sizeBoard == 19 = 2
  | otherwise = 1
