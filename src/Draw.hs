module Draw where

import Graphics.Gloss.Interface.Pure.Game
import Config
import Models

import Data.Char
-- import Data.Generics.Any
import qualified Data.Map as Map

-- =========================================
-- Отрисовка игры
-- =========================================

-- | Отрисовка игры, складывает изображения сетки и поля.
drawGame :: Game -> Picture
drawGame game = translate (-w) (-h) (scale c c (pictures
  [ drawGrid
  , drawBoard (gameBoard game)
  -- , drawPass (numberOfPass game)
  , drawScores (gameScore game)
  -- , drawStones (scoreStones game)
  , drawEndGame (gameWinner game) (scoreStones game) (endGame game)
  ]))
  where
    c = fromIntegral cellSize
    w = fromIntegral screenWidth  / 2 - offset
    h = fromIntegral screenHeight / 2 - offset
    offset = fromIntegral screenOffset / 2

-- | Отрисовка количества пассов
drawPass :: Passes -> Picture
drawPass (x,y) = (scale 0.005 0.005 (text [(intToDigit x), (intToDigit y)]))

-- | Отрисовка количества очков игры
drawScores :: Scores -> Picture
drawScores (x, y) = translate w h (scale 0.005 0.005 (text [(intToDigit (round x)), (intToDigit  (round y))] ))
  where
    w = fromIntegral screenWidth / 200
    h = fromIntegral screenHeight / 200

drawEndGame :: Maybe Stone -> AmountStones -> Maybe Float -> Picture
drawEndGame _ _ Nothing = blank
drawEndGame _ _ (Just c) = (scale c c (text "END GAME"))

--
drawStones :: AmountStones -> Picture
drawStones (x, y) = translate w h (scale 0.005 0.005 (text [(intToDigit x), (intToDigit y)] ))
  where
    w = fromIntegral screenWidth / 200
    h = fromIntegral screenHeight / 200

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


-- | Отрисовка экранов
drawScreen :: Screen -> Picture
drawScreen (ScreenLogin loginScreen) = drawLoginScreen loginScreen
drawScreen (ScreenMainMenu mainMenuScreen) = drawMainMenuScreen mainMenuScreen
drawScreen (ScreenGame game) = drawGame game
drawScreen (ScreenRegistration registrationScreen) = drawRegistrationScreen registrationScreen
drawScreen (ScreenRecords recordsScreen) = drawRecordsScreen recordsScreen
drawScreen (ScreenSettings settingsScreen) = drawSettingsScreen settingsScreen

-- | На какую клавишу навели курсор на экране ввода логина и пароля
changeLoginButton :: Point -> Maybe LoginButton
changeLoginButton point
  | isButtonMenu point enterHeight == True = (Just Enter)
  | isButtonMenu point registrationHeight == True = (Just Registration)
  | isButtonMenu point loginHeight == True = (Just Login)
  | isButtonMenu point passwordHeight == True = (Just Password)
  | otherwise = Nothing

-- | На какую клавишу навели курсор на экране главного меню
changeMainMenuButton :: Point -> Maybe MenuButton
changeMainMenuButton point
  | isButtonMenu point newGameHeight == True = (Just NewGame)
  | isButtonMenu point recordsHeight == True = (Just Records)
  | isButtonMenu point settingsHeight == True = (Just Settings)
  | otherwise = Nothing

-- | На какую клавишу навели курсор на экране регистрации
changeRegistrationButton :: Point -> Maybe RegistrationButton
changeRegistrationButton point
  | isButtonMenu point enterHeight == True = (Just EnterRegistration)
  | isButtonMenu point loginHeight == True = (Just LoginRegistration)
  | isButtonMenu point passwordHeight == True = (Just PasswordRegistration)
  | isButtonMenu point exitHeight == True = (Just ExitRegistration)
  | otherwise = Nothing

-- | На какую клавишу навели курсор на экране рекордов
changeRecordsButton :: Point -> Maybe RecordsButton
changeRecordsButton point
  | isButtonMenu point exitHeight == True = (Just ExitRecords)
  | otherwise = Nothing

-- | На какую клавишу навели курсор на экране настроек
changeSettingsButton :: Point -> Maybe SettingsButton
changeSettingsButton point
  | isButton point x0 yPlayers0 x1 yPlayers1 == True = (Just PlayersGameType)
  | isButton point x0 yAI0 x1 yAI1 == True = (Just AIGameType)
  | isButtonMenu point exitHeight == True = (Just ExitSettings)
  | otherwise = Nothing
  where
    x0 = gameTypeVarX - 10
    x1 = x0 + menuItemWidth
    yPlayers0 = gameTypeSettingsHeight - 20
    yPlayers1 = yPlayers0 + menuItemHeight
    yAI0 = gameTypeSettingsHeight - menuItemHeight - 30
    yAI1 = yAI0 + menuItemHeight

-- | Была нажата кнопка мыши на экране ввода логина и пароля => сменить экран
changeLoginScreen :: Point -> Screen -> Screen
changeLoginScreen point (ScreenLogin screen)
  | (loginFocusButton screen) == Nothing
      = ScreenLogin LoginScreen
        { loginFocusButton = Nothing
        , loginState = Nothing
        , name = (name screen)
        , password = (password screen)
        }
  | (loginFocusButton screen) == (Just Login)
      = ScreenLogin LoginScreen
        { loginFocusButton = (Just Login)
        , loginState = (Just LoginState)
        , name = (name screen)
        , password = (password screen)
        }
  | (loginFocusButton screen) == (Just Password)
      = ScreenLogin LoginScreen
        { loginFocusButton = (Just Password)
        , loginState = (Just PasswordState)
        , name = (name screen)
        , password = (password screen)
        }
  | (loginFocusButton screen) == (Just Registration)
      = ScreenRegistration RegistrationScreen
        { registrationButton = Nothing
        , registrationState = Nothing
        , newName = ""
        , newPassword = ""
        }
  | (loginFocusButton screen) == (Just Enter) &&
                                 (length (name screen) > 0) &&
                                 (length (password screen) > 0)
      = ScreenMainMenu MainMenuScreen
        { menuFocusButton = Nothing
        , gameType = PlayersGame
        }
  | otherwise
      = ScreenLogin LoginScreen
        { loginFocusButton = changeLoginButton point --Nothing
        , loginState = Nothing
        , name = (name screen)
        , password = (password screen)
        }
changeLoginScreen _ screen = screen

-- | Была нажата кнопка мыши на экране регистрации => сменить экран
changeRegistrationScreen :: Point -> Screen -> Screen
changeRegistrationScreen point (ScreenRegistration screen)
  | (registrationButton screen) == Nothing
      = ScreenRegistration RegistrationScreen
        { registrationButton = Nothing
        , registrationState = Nothing
        , newName = (newName screen)
        , newPassword = (newPassword screen)
        }
  | (registrationButton screen) == (Just LoginRegistration)
      = ScreenRegistration RegistrationScreen
        { registrationButton = (Just LoginRegistration)
        , registrationState = (Just LoginState)
        , newName = (newName screen)
        , newPassword = (newPassword screen)
        }
  | (registrationButton screen) == (Just PasswordRegistration)
      = ScreenRegistration RegistrationScreen
        { registrationButton = (Just PasswordRegistration)
        , registrationState = (Just PasswordState)
        , newName = (newName screen)
        , newPassword = (newPassword screen)
        }
  | (registrationButton screen) == (Just EnterRegistration) &&
                                   (length (newName screen) > 0) &&
                                   (length (newPassword screen) > 0)
      = ScreenLogin LoginScreen
        { loginFocusButton = Nothing
        , loginState = Nothing
        , name = "" --(name screen)
        , password = "" -- (password screen)
        }
  | (registrationButton screen) == (Just ExitRegistration)
      = ScreenLogin LoginScreen
        { loginFocusButton = Nothing
        , loginState = Nothing
        , name = ""
        , password = ""
        }
   | otherwise
      = ScreenRegistration RegistrationScreen
        { registrationButton = changeRegistrationButton point
        , registrationState = Nothing
        , newName = (newName screen)
        , newPassword = (newPassword screen)
        }
changeRegistrationScreen _ screen = screen

-- | Была нажата кнопка мыши на экране главного меню => сменить экран
changeMainMenuScreen :: Screen -> Screen
changeMainMenuScreen (ScreenMainMenu screen) =
  case (menuFocusButton screen) of
    Nothing -> ScreenMainMenu MainMenuScreen
      { menuFocusButton = Nothing
      , gameType = (gameType screen)
      }
    (Just NewGame) -> ScreenGame initGame
    (Just Records) -> ScreenRecords RecordsScreen
      { recordsButton = Nothing
      , recordsGameType = (gameType screen)
      }
    (Just Settings) -> ScreenSettings SettingsScreen
      { settingsButton = Nothing
      , settingsGameType = (gameType screen)
      }
changeMainMenuScreen screen = screen

-- | Была нажата кнопка мыши на экране рекордов => сменить экран
changeRecordsScreen :: Screen -> Screen
changeRecordsScreen (ScreenRecords screen) =
  case (recordsButton screen) of
    Nothing -> ScreenRecords RecordsScreen
      { recordsButton = Nothing
      , recordsGameType = (recordsGameType screen)
      }
    (Just ExitRecords) -> ScreenMainMenu MainMenuScreen
      { menuFocusButton = Nothing
      , gameType = (recordsGameType screen)
      }
changeRecordsScreen screen = screen

-- | Была нажата кнопка мыши на экране настроек => сменить экран
changeSettingsScreen :: Screen -> Screen
changeSettingsScreen (ScreenSettings screen) =
  case (settingsButton screen) of
    Nothing -> ScreenSettings SettingsScreen
      { settingsButton = Nothing
      , settingsGameType = (settingsGameType screen)
      }
    (Just ExitSettings) -> ScreenMainMenu MainMenuScreen
      { menuFocusButton = Nothing
      , gameType = (settingsGameType screen)
      }
    (Just AIGameType) -> ScreenSettings SettingsScreen
      { settingsButton = (Just AIGameType)
      , settingsGameType = AIGame
      }
    (Just PlayersGameType) -> ScreenSettings SettingsScreen
      { settingsButton = (Just PlayersGameType)
      , settingsGameType = PlayersGame
      }
changeSettingsScreen screen = screen

-- | Курсор изменил положение => изменить состояние
checkScreen :: Point -> Screen -> Screen
checkScreen point (ScreenLogin screen) =
  ScreenLogin LoginScreen
    { loginFocusButton = changeLoginButton point
    , loginState = (loginState screen)
    , name = (name screen)
    , password = (password screen)
    }
checkScreen point (ScreenRegistration screen) =
  ScreenRegistration RegistrationScreen
    { registrationButton = changeRegistrationButton point
    , registrationState = (registrationState screen)
    , newName = (newName screen)
    , newPassword = (newPassword screen)
    }
checkScreen point (ScreenMainMenu screen) =
  ScreenMainMenu MainMenuScreen
    { menuFocusButton = changeMainMenuButton point
    , gameType = (gameType screen)
    }
checkScreen point (ScreenRecords screen) =
  ScreenRecords RecordsScreen
    { recordsButton = changeRecordsButton point
    , recordsGameType = (recordsGameType screen)
    }
checkScreen point (ScreenSettings screen) =
  ScreenSettings SettingsScreen
    { settingsButton = changeSettingsButton point
    , settingsGameType = (settingsGameType screen)
    }
checkScreen _ screen = screen

-----------------------------  D R A W   R E G I S T R A T I O N  --------------------------------

-- | Отрисовка экрана регистрации
drawRegistrationScreen :: RegistrationScreen -> Picture
drawRegistrationScreen screen = pictures
  [ drawLogin (newName screen)
  , drawPassword (newPassword screen)
  , drawEnter
  , drawExit
  , writeBorder (registrationState screen)
  , allocateBorderRegistration (registrationButton screen)
  ]

-- | Отрисовка границы поля ввода
writeBorder :: Maybe WriteState -> Picture
writeBorder Nothing              = blank
writeBorder (Just LoginState)    = translate (-menuItemWidth / 2) loginHeight drawMenuBorder
writeBorder (Just PasswordState) = translate (-menuItemWidth / 2) passwordHeight drawMenuBorder

-- | Граница кнопки экрана регистрации
allocateBorderRegistration :: Maybe RegistrationButton -> Picture
allocateBorderRegistration Nothing                     = blank
allocateBorderRegistration (Just EnterRegistration)    = translate (-menuItemWidth / 2) enterHeight drawMenuBorder
allocateBorderRegistration (Just ExitRegistration)     = translate (-menuItemWidth / 2) exitHeight drawMenuBorder
allocateBorderRegistration (Just LoginRegistration)    = translate (-menuItemWidth / 2) loginHeight drawMenuBorder
allocateBorderRegistration (Just PasswordRegistration) = translate (-menuItemWidth / 2) passwordHeight drawMenuBorder


-----------------------------  D R A W   R E C O R D S  --------------------------------

-- | Отрисовка экрана регистрации
drawRecordsScreen :: RecordsScreen -> Picture
drawRecordsScreen screen = pictures
  [ drawHeader "Records"
  , drawExit
  , writeRecords
  , allocateBorderRecords (recordsButton screen)
  ]

-- | Граница кнопки экрана рекордов
allocateBorderRecords :: Maybe RecordsButton -> Picture
allocateBorderRecords Nothing = blank
allocateBorderRecords (Just ExitRecords) = translate (-menuItemWidth / 2) exitHeight drawMenuBorder

-- | Забираем список рекордов
writeRecords :: Picture
writeRecords = writeListRecords initUsers 0 -- queryTableRec

-- | Это не нужно
initUsers :: Users
initUsers = initUser1 : initUser2 : initUser1 : initUser3 : initUser4 : initUser2 : []

-- | Это не нужно
initUser1 :: User
initUser1 = ("abc", "15")

-- | Это не нужно
initUser2 :: User
initUser2 = ("1234567890", "1234567890")

-- | Это не нужно
initUser3 :: User
initUser3 = ("defs", "1df5")

-- | Это не нужно
initUser4 :: User
initUser4 = ("1234jh", "4567890")

-- | Пишем список рекордов
writeListRecords :: Users -> Float -> Picture
writeListRecords [] _ = blank
writeListRecords ((name1, password1):xs) num
  | num < 5 = pictures
    [ pictureName1
    , pictureName2
    , pictureScore
    , writeListRecords xs (num + 1)
    ]
  | otherwise = blank
  where
    pictureName1 = translate recordsIndent y scalePictureName
    pictureName2 = translate (recordsIndent + nameLength*16) y scalePictureName
    pictureScore = translate (recordsIndent + nameLength*32) y scalePictureScore
    scalePictureName  = (scale strScaleWidth strScaleHeight (color black (text name1)))
    scalePictureScore = (scale strScaleWidth strScaleHeight (color black (text password1)))
    y = firstRecordHeight - 50*num


-----------------------------  D R A W   S E T T I N G S  --------------------------------

-- | Отрисовка экрана настроек
drawSettingsScreen :: SettingsScreen -> Picture
drawSettingsScreen screen = pictures
  [ drawHeader "Settings"
  , drawExit
  , writeGameType
  , gameTypeSelectedBorder (settingsGameType screen)
  , allocateBorderSettings (settingsButton screen)
  ]

-- | Граница кнопки экрана настроек
allocateBorderSettings :: Maybe SettingsButton -> Picture
allocateBorderSettings (Just ExitSettings)
  = translate (-menuItemWidth / 2) exitHeight drawMenuBorder
allocateBorderSettings (Just PlayersGameType)
  = translate x y1 drawMenuBorder
  where
    x = gameTypeVarX - 10
    y1 = gameTypeSettingsHeight - 20
allocateBorderSettings (Just AIGameType) = translate x y2 drawMenuBorder
  where
    x = gameTypeVarX - 10
    y2 = gameTypeSettingsHeight - menuItemHeight - 30
allocateBorderSettings Nothing = blank


-- | Граница кнопки выбранной настройки
gameTypeSelectedBorder :: GameType -> Picture
gameTypeSelectedBorder PlayersGame = translate x y1 drawMenuBorder
  where
    x = gameTypeVarX - 10
    y1 = gameTypeSettingsHeight - 20
gameTypeSelectedBorder AIGame = translate x y2 drawMenuBorder
  where
    x = gameTypeVarX - 10
    y2 = gameTypeSettingsHeight - menuItemHeight - 30

-- | Выписываем названия кнопок настроек игры
writeGameType :: Picture
writeGameType = pictures
  [ drawItem (gameTypeVarX - 10) (gameTypeSettingsHeight - 20) buttonColor
  , drawItem (gameTypeVarX - 10) (gameTypeSettingsHeight - menuItemHeight - 30) buttonColor
  , writeStr gameTypeStrX gameTypeSettingsHeight "Game type"
  , writeStr gameTypeVarX gameTypeSettingsHeight "Two players"
  , writeStr gameTypeVarX (gameTypeSettingsHeight - 60) "AI"
  ]

-- | Заголовок
drawHeader :: String -> Picture
drawHeader str = writeMenuStr (fromIntegral (length str)) height str
  where
    height = headerHeight + menuItemHeight / 2.5

-- | Выход
drawExit :: Picture
drawExit = pictures
  [ (drawMenuItem exitHeight buttonColor)
  , writeMenuStr 4 height "Exit"
  ]
   where
      height = exitHeight + menuItemHeight / 2.5

-- | Отрисовка экрана ввода логина и пароля
drawLoginScreen :: LoginScreen -> Picture
drawLoginScreen screen = pictures
  [ drawLogin (name screen)
  , drawPassword (password screen)
  , drawEnter
  , drawRegistration
  , writeBorder (loginState screen)
  , allocateBorder1 (loginFocusButton screen)
  ]

-- | Отрисовка главного меню
drawMainMenuScreen :: MainMenuScreen -> Picture
drawMainMenuScreen screen = pictures
  [ drawNewGame
  , drawRecords
  , drawSettings
  , allocateBorder2 (menuFocusButton screen)
  ]

-- | Построение прямоугольника по координатам
drawItem :: Float -> Float -> Color -> Picture
drawItem x0 y0 myColor = picture
  where
    picture = color myColor figure
    figure = polygon
      [ (x0, y0)
      , (x0 + menuItemWidth, y0)
      , (x0 + menuItemWidth, y0 + menuItemHeight)
      , (x0, y0 + menuItemHeight)
      ]


-- | Построение прямоугольника - пункта меню
drawMenuItem :: Float -> Color -> Picture
drawMenuItem height myColor = translate (-menuItemWidth / 2) height picture
  where
    picture = color myColor figure
    figure = polygon
      [ (0.0, 0.0)
      , (menuItemWidth, 0.0)
      , (menuItemWidth, menuItemHeight)
      , (0.0, menuItemHeight)
      ]

-- | Отрисовка окна ввода логина
drawLogin :: Name -> Picture
drawLogin str = pictures
  [ (drawMenuItem loginHeight white)
  , writeMenuStr (fromIntegral (length str)) height str
  , writeMenuStr 5 (loginHeight - 100*strScaleHeight) "Login"
  ]
  where
    height = loginHeight + menuItemHeight / 2.5

-- | Отрисовка окна ввода пароля
drawPassword :: Password -> Picture
drawPassword str = pictures
  [ (drawMenuItem passwordHeight white)
  , writeMenuStr (fromIntegral (length str)) height str
  , writeMenuStr 8 (passwordHeight - 100*strScaleHeight) "Password"
  ]
  where
    height = passwordHeight + menuItemHeight / 2.5

-- | Отрисовка кнопки Enter
drawEnter :: Picture
drawEnter = pictures
  [ (drawMenuItem enterHeight buttonColor)
  , writeMenuStr 5 height "Enter"
  ]
  where
    height = enterHeight + menuItemHeight / 2.5

-- | Отрисовка кнопки Registration
drawRegistration :: Picture
drawRegistration = pictures
  [ (drawMenuItem registrationHeight buttonColor)
  , writeMenuStr 12 height "Registration"
  ]
  where
    height = registrationHeight + menuItemHeight / 2.5

-- | Отрисовка кнопки New game
drawNewGame :: Picture
drawNewGame = pictures
  [ (drawMenuItem newGameHeight buttonColor)
  , writeMenuStr 8 height "New game"
  ]
  where
    height = newGameHeight + menuItemHeight / 2.5

-- | Отрисовка кнопки Records
drawRecords :: Picture
drawRecords = pictures
  [ (drawMenuItem recordsHeight buttonColor)
  , writeMenuStr 7 height "Records"
  ]
  where
    height = recordsHeight + menuItemHeight / 2.5

-- | Отрисовка кнопки Settings
drawSettings :: Picture
drawSettings = pictures
  [ (drawMenuItem settingsHeight buttonColor)
  , writeMenuStr 8 height "Settings"
  ]
  where
    height = settingsHeight + menuItemHeight / 2.5

-- | Подпись в меню
writeMenuStr :: Float -> Float -> String -> Picture
writeMenuStr letters height str = translate x height (scale strScaleWidth strScaleHeight picture)
  where
    picture = color black (text str)
    x = -letters * 13 / 2

-- | Подпись
writeStr :: Float -> Float -> String -> Picture
writeStr x y str = translate x y (scale strScaleWidth strScaleHeight picture)
  where
    picture = color black (text str)

-- | Отрисовка границы пункта меню
drawMenuBorder :: Picture
drawMenuBorder = color black (line [ (0.0, 0.0)
                                   , (menuItemWidth, 0.0)
                                   , (menuItemWidth, menuItemHeight)
                                   , (0.0, menuItemHeight), (0.0, 0.0)
                                   ]
                             )

-- | Граница кнопки экрана ввода логина и пароля
allocateBorder1 :: Maybe LoginButton -> Picture
allocateBorder1 Nothing             = blank
allocateBorder1 (Just Login)        = translate (-menuItemWidth / 2) loginHeight drawMenuBorder
allocateBorder1 (Just Password)     = translate (-menuItemWidth / 2) passwordHeight drawMenuBorder
allocateBorder1 (Just Registration) = translate (-menuItemWidth / 2) registrationHeight drawMenuBorder
allocateBorder1 (Just Enter)        = translate (-menuItemWidth / 2) enterHeight drawMenuBorder

-- | Граница кнопки экрана главного меню
allocateBorder2 :: Maybe MenuButton -> Picture
allocateBorder2 Nothing         = blank
allocateBorder2 (Just NewGame)  = translate (-menuItemWidth / 2) newGameHeight drawMenuBorder
allocateBorder2 (Just Records)  = translate (-menuItemWidth / 2) recordsHeight drawMenuBorder
allocateBorder2 (Just Settings) = translate (-menuItemWidth / 2) settingsHeight drawMenuBorder

-- | Принадлежит ли точка заданной прямоугольной области
isButton :: Point -> Float -> Float -> Float -> Float -> Bool
isButton (x, y) x0 y0 x1 y1 =
  (x >= x0) &&
  (x <= x1) &&
  (y >= y0) &&
  (y <= y1)


-- | Принадлежит ли точка клавише меню, заданной вторым параметром - высотой кнопки
isButtonMenu :: Point -> Float -> Bool
isButtonMenu (x, y) height =
  (x >= -menuItemWidth / 2) &&
  (x <= menuItemWidth / 2) &&
  (y >= height) &&
  (y <= height + menuItemHeight)
