module Game  where
import Data.Monoid()
import Data.Foldable()

import Graphics.Gloss.Interface.Pure.Game
import Data.Map (Map)
import qualified Data.Map as Map

run :: IO()
run = do
    play display bgColor fps initScreen drawScreen handleScreen updateScreen
  where
    display = InWindow "Game Go" (screenWidth, screenHeight) (200, 200)
    bgColor = makeColorI 245 245 220 255 -- цвет фона, бежевый
    fps     = 60      -- кол-во кадров в секунду

 
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

-- | Начальное состояние экрана - экран ввода логина и пароля
initScreen :: Screen
initScreen = ScreenLogin LoginScreen
  { loginFocusButton = Nothing        -- Кнопка никакая еще не выделена
  , loginState = Nothing              -- Не было входа в состояния ввода
  , name = ""                         -- Имя еще не введено
  , password = ""                     -- Пароль еще не введен
  }

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
handleMainMenuScreen (EventKey (MouseButton LeftButton) _ _ _) screen 
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

----------------------   D R A W     S C R E E N S   ------------------------------
-- | Ширина пункта меню
menuItemWidth :: Float 
menuItemWidth = 200.0

-- | Высота пункта меню
menuItemHeight :: Float 
menuItemHeight = 50.0

-- | Ширина сжатия текста
strScaleWidth :: Float
strScaleWidth = 0.20

-- | Высота сжатия текста
strScaleHeight :: Float
strScaleHeight = 0.15

-- | Высота расположения поля ввода логина
loginHeight :: Float
loginHeight = 150

-- | Высота расположения поля ввода пароля
passwordHeight :: Float
passwordHeight = 40

-- | Длина строки с именем
nameLength :: Float
nameLength = 10

-- | Длина строки с паролем
passwordLength :: Float
passwordLength = 10

-- | Высота расположения кнопки Enter
enterHeight :: Float
enterHeight = -80

-- | Высота расположения кнопки Registration
registrationHeight :: Float
registrationHeight = -150

-- | Высота расположения кнопки New game
newGameHeight :: Float
newGameHeight = 130

-- | Высота расположения кнопки Record
recordsHeight :: Float
recordsHeight = 60

-- | Высота расположения кнопки Settings
settingsHeight :: Float
settingsHeight = -10

-- | Высота расположения кнопки Exit
exitHeight :: Float
exitHeight = -200

-- | Высота расположения заголовков
headerHeight :: Float
headerHeight = 190

-- | Отступ слева при записи рекордов
recordsIndent :: Float
recordsIndent = -220

-- | Высота первого рекорда
firstRecordHeight :: Float
firstRecordHeight = 150

-- | Высота настроек
gameTypeSettingsHeight :: Float
gameTypeSettingsHeight = 150

-- | Координата настроек игры
gameTypeStrX :: Float
gameTypeStrX = -200

-- | Координата кнопок настроек игры
gameTypeVarX :: Float
gameTypeVarX = -10

-- | Цвет кнопок
buttonColor :: Color
buttonColor = makeColorI 220 220 190 255 
  --  245 245 220 255 Цвет фона

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

------------------------------------------------------------------------------------------

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

-- | Обработка пассов
takePass :: Game -> Game
takePass game
  | np == (1,1) = checkGroups game
  | np == (2,2) = gameOver game
  | otherwise = game {gamePlayer = switchPlayer (gamePlayer game)}
  where np = (numberOfPass game)
-- | Количество очков двух игроков.
type Scores = (Score, Score)

-- | Наша точка.
type Node = (Int, Int)

-- | Игровое поле.
type Board = Map Node Cell
gameOver :: Game -> Game
gameOver game = game
  { gameWinner = winner game
  , endGame = Just 0.005
  }

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
  , gameComi :: Float -- количество форы
  , gameWinner :: Maybe Stone -- победитель?!
  , gameBoard :: Board
  , listBoard :: [Board] -- список всех предыдущих состояний
  , scoreStones :: AmountStones -- кол-во камней убранных каждым игроком
  -- , numberOfPass ::
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
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) _ _ mouse) = placeStone (mouseToCell mouse)
handleGame (EventMotion mouse) = placeShadowStone (mouseToCell mouse)
--handleGame (EventKey (Char c) Down _ _ ) _ = readStr c screen
-- handleGame (EventKey (SpecialKey KeySpace) _ _ _) = takePass
handleGame _ = id

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

-- | Применяем изменения, которые произошли на доске.
changeBoard :: Board -> Game -> Game
changeBoard board game = game
  { gameBoard  = board
  , listBoard = setBoard (gameBoard game) (listBoard game) -- будет ли это работать?
  }

-- | Закончить ход, инициализировать состояние игры для нового хода.
completeMove :: Game -> Game
completeMove game = game
  { gamePlayer = switchPlayer (gamePlayer game)
  , gameScore = amountScores (gameBoard game) + float (scoreStones game)
  , listBoard = setBoard (gameBoard game) (listBoard game)
  , numberOfPass = (0, 0)
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


-- | Правило Ко борьбы, true если все по правилам.
-- Конкретно данное состояние встретилось менее трех раз и оно не совпало с предыдущим => все норм
ruleKo :: Node -> Stone -> Board -> [Board] -> Bool
ruleKo _ _ _ [] = True
ruleKo point stone board (x:xs)
--  = amountEqBoards board (x:xs) < 1
--  && boardTry (place point stone board) (x:xs) /= x
  = boardTry stone (place point stone board) (x:xs) /= x

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
  }

-- | Сколько раз встречалась такая доска раньше.
amountEqBoards :: Board -> [Board] -> Int
amountEqBoards board = length . filter (== board)

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
      scoreblack game = fst (gameScore game) + fromIntegral( fst ( scoreStones game))
      scorewhite game = (gameComi game) + snd (gameScore game) + fromIntegral( snd ( scoreStones game))

-- | Обновление игры.
-- В этой игре все изменения происходят только по действиям игрока,
-- поэтому функция обновления — это тождественная функция.
updateGame :: Float -> Game -> Game
updateGame _ = id

-- =========================================
-- Константы, параметры игры.
-- =========================================

-- | Главная функция
ai :: Stone -> Board -> Maybe Move
ai _ _ = Nothing
-- ai stone board = fromBestMove . fold . bestMoves . fmap estimate . cutTree n . gameTree
-- более умная свертка fold
-- | Радиус картинки камня.
radiusStone :: Float
radiusStone = 0.45

-- minimax :: GameTree (m, e) -> m
-- minimax

-- | Возможные ходы
possibleMoves :: Stone -> Board -> [Move]
possibleMoves _ _ = []
-- | Начальная фора(очки) белого игрока
-- может варьироваться уже по желания, но это позже в индивидуальных частях.
playerComi :: Float
playerComi = 6.5

-- | Построение дерева игры
gameTree :: Board -> GameTree Board
gameTree a = Leaf a
-- | начальная фора(камни) черного игрока, надо подумать как это реализовать
-- обозначает, сколько камней должен поставит черный игрок перед началом партии
-- это нужно если очковой форы не хватает и игроки слишком разного уровня
-- но не более 9(для доски 19x19), для 9x9 будет не больше 4.
playerHandicap :: Int
playerHandicap = 0

-- | Ширина игрового поля в клетках.
boardWidth :: Int
boardWidth  = 9

-- | Обрезание дерева игры
-- cutTree :: Int -> GameTree b a -> GameTree b a
cutTree :: Int -> GameTree a -> GameTree a
cutTree _ (Leaf a) = Leaf a
cutTree n tree@(Node b trees)
  | n == 0 = Leaf b
  | otherwise = Node b $ map (\(m, t) -> (m, cutTree (n-1) t)) trees
-- | Высота игрового поля в клетках.
boardHeight :: Int
boardHeight = 9

-- | Оценка игрового поля
estimate :: Board -> Estimate
estimate _ = Estimate 0 0 0.0
-- | Размер одной клетки в пикселях.
cellSize :: Int
cellSize = 50

-- | Лучшие ходы
-- bestMoves :: GameTree b Estimate -> GameTree b BestMove
bestMoves :: GameTree Estimate -> GameTree BestMove
bestMoves (Leaf _) = (Leaf NoMove)
-- bestMoves (Node ts) = Node $ map (\(m, t) -> (m, fmap (BestMove m) t) ) ts

-- | Отступы от края экрана.
screenOffset :: Int
screenOffset = 50

-- | Ширина экрана в пикселях.
screenWidth :: Int
screenWidth  = cellSize * boardWidth + screenOffset

-- | Высота экрана в пикселях.
screenHeight :: Int
screenHeight = cellSize * boardHeight + screenOffset
