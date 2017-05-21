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


type Name = String
type Password = String
type User = (Name, Password)
type Users = [User]


data ScreenState = FirstScreenState | SecondScreenState | RegistrationScreenState | RecordsScreenState | SettingsScreenState | GameScreenState
  deriving(Eq, Show)

data FirstScreenState = Enter | Login | Password | Registration | EnterBorder | RegistrationBorder | LoginBorder | PasswordBorder | NoFirstState
  deriving(Eq, Show)

data SecondScreenState = NewGame | Records | Settings | NewGameBorder | RecordsBorder | SettingsBorder | NoSecondState
  deriving(Eq, Show)

data RecordsScreenState = ExitRecords | ExitRecordsBorder | NoRecordsState
  deriving(Eq, Show)

data SettingsScreenState = ExitSettings | ExitSettingsBorder | AIGameBorder | PlayersGameBorder | NoSettingsState
  deriving(Eq, Show)

data GameType = AIGame | PlayersGame
  deriving(Eq, Show)

data RegistrationScreenState = ExitRegistration | LoginRegistration | PasswordRegistration | EnterRegistration | ExitRegistrationBorder | LoginRegistrationBorder | PasswordRegistrationBorder | EnterRegistrationBorder | NoRegistrationState
  deriving(Eq, Show)

data Screen = Screen
  { screenState :: ScreenState
  , user :: User 
  , firstScreen :: FirstScreen
  , secondScreen :: SecondScreen
  , registrationScreen :: RegistrationScreen
  , recordsScreen :: RecordsScreen
  , settingsScreen :: SettingsScreen
  , game :: Game
  }

data FirstScreen = FirstScreen
  { state1 :: FirstScreenState 
  , name :: Name
  , password :: Password
  }

data SecondScreen = SecondScreen
  { state2 :: SecondScreenState 
--  , tryUser :: 
  }

data RegistrationScreen = RegistrationScreen
  { stateRegistration :: RegistrationScreenState 
  , newName :: Name
  , newPassword :: Password
  }

data RecordsScreen = RecordsScreen
  { stateRecords :: RecordsScreenState
  , users :: [User]
  }

data SettingsScreen = SettingsScreen
  { stateSettings :: SettingsScreenState
  , gameType :: GameType
  }

initScreen :: Screen
initScreen = Screen
  { screenState = FirstScreenState
  , user = ("", "")
  , firstScreen = initFirstScreen
  , secondScreen = initSecondScreen
  , registrationScreen = initRegistrationScreen
  , recordsScreen = initRecordsScreen
  , settingsScreen = initSettingsScreen
  , game = initGame
  }

drawScreen :: Screen -> Picture
drawScreen screen = 
    case screenState screen of
       FirstScreenState -> drawFirstScreen (firstScreen screen)
       SecondScreenState -> drawSecondScreen (secondScreen screen)
       RegistrationScreenState -> drawRegistrationScreen (registrationScreen screen)
       RecordsScreenState -> drawRecordsScreen (recordsScreen screen)
       SettingsScreenState -> drawSettingsScreen (settingsScreen screen)
       GameScreenState -> drawGame (game screen)

handleScreen :: Event -> Screen -> Screen
handleScreen event screen =
    case screenState screen of
       FirstScreenState -> Screen 
             { screenState = makeScreenState event screen
             , user = (user screen)  -- ("", "")
             , firstScreen = handleFirstScreen event (firstScreen screen)
             , secondScreen = (secondScreen screen) 
             , registrationScreen = RegistrationScreen { stateRegistration = NoRegistrationState
                                                       , newName = ""
                                                       , newPassword = ""
                                                       }
             , recordsScreen = (recordsScreen screen)
             , settingsScreen = (settingsScreen screen)
             , game = (game screen)
             }
       SecondScreenState -> Screen 
             { screenState = makeScreenState event screen 
             , user = (user screen)  -- ("", "")
             , firstScreen = (firstScreen screen)
             , secondScreen = handleSecondScreen event (secondScreen screen)
             , registrationScreen = (registrationScreen screen)
             , recordsScreen = RecordsScreen { stateRecords = NoRecordsState
                                             , users = (users (recordsScreen screen))
                                             }
             , settingsScreen = SettingsScreen { stateSettings = NoSettingsState
                                               , gameType = (gameType (settingsScreen screen))
                                               }
             , game = (game screen)
             }
       RegistrationScreenState -> Screen 
             { screenState = makeScreenState event screen
             , user = (user screen)  -- ("", "")
             , firstScreen = (firstScreen screen)
             , secondScreen = (secondScreen screen) 
             , registrationScreen = handleRegistrationScreen event (registrationScreen screen)
             , recordsScreen = (recordsScreen screen)
             , settingsScreen = (settingsScreen screen)
             , game = (game screen)
             }
       RecordsScreenState -> Screen 
             { screenState = makeScreenState event screen
             , user = (user screen)  -- ("", "")
             , firstScreen = (firstScreen screen)
             , secondScreen = (secondScreen screen) 
             , registrationScreen = (registrationScreen screen)
             , recordsScreen = handleRecordsScreen event (recordsScreen screen)
             , settingsScreen = (settingsScreen screen)
             , game = (game screen)
             }
       SettingsScreenState -> Screen 
             { screenState = makeScreenState event screen
             , user = (user screen)  -- ("", "")
             , firstScreen = (firstScreen screen)
             , secondScreen = (secondScreen screen) 
             , registrationScreen = (registrationScreen screen)
             , recordsScreen = (recordsScreen screen)
             , settingsScreen = handleSettingsScreen event (settingsScreen screen)
             , game = (game screen)
             }
       GameScreenState -> Screen 
             { screenState = GameScreenState
             , user = (user screen)  -- ("", "")
             , firstScreen = (firstScreen screen)
             , secondScreen = (secondScreen screen) 
             , registrationScreen = (registrationScreen screen)
             , recordsScreen = (recordsScreen screen)
             , settingsScreen = (settingsScreen screen)
             , game = handleGame event (game screen)
             }

makeScreenState :: Event -> Screen -> ScreenState
makeScreenState event screen | (screenState screen) == GameScreenState = GameScreenState
                             | otherwise = (analyseScreenState1 event screen)

analyseScreenState1 :: Event -> Screen -> ScreenState
analyseScreenState1 (EventKey (MouseButton LeftButton) _ _ mouse) screen 
          | ((state1 (firstScreen screen)) == Enter) = analyseScreenState2 screen
          | ((state1 (firstScreen screen)) == Registration) = analyseScreenStateRegistration screen
          | otherwise  = FirstScreenState
analyseScreenState1 _ screen = (screenState screen)

analyseScreenStateRegistration :: Screen -> ScreenState
analyseScreenStateRegistration screen
          | (stateRegistration (registrationScreen screen)) == NoRegistrationState = RegistrationScreenState
          | (stateRegistration (registrationScreen screen)) == EnterRegistration = SecondScreenState
          | (stateRegistration (registrationScreen screen)) == ExitRegistration = FirstScreenState
          | otherwise = RegistrationScreenState

analyseScreenState2 :: Screen -> ScreenState
analyseScreenState2 screen 
          | (state2 (secondScreen screen)) == NoSecondState = SecondScreenState
          | (state2 (secondScreen screen)) == NewGame = GameScreenState
          | (state2 (secondScreen screen)) == Records = analyseRecordsState screen
          | (state2 (secondScreen screen)) == Settings = analyseSettingsState screen
          | otherwise  = SecondScreenState

analyseRecordsState :: Screen -> ScreenState
analyseRecordsState screen | (stateRecords (recordsScreen screen)) == ExitRecords = SecondScreenState
                           | otherwise = RecordsScreenState

analyseSettingsState :: Screen -> ScreenState
analyseSettingsState screen | (stateSettings (settingsScreen screen)) == ExitSettings = SecondScreenState
                            | otherwise = SettingsScreenState

updateScreen :: Float -> Screen -> Screen
updateScreen _ = id

----------------------------------------------

initFirstScreen :: FirstScreen
initFirstScreen = FirstScreen
  { state1 = NoFirstState
--  , tryUser = ("", "")
  , name = ""
  , password = "" 
  }

initSecondScreen :: SecondScreen
initSecondScreen = SecondScreen
  { state2 = NoSecondState
--  , tryUser = ("", "")
  }

initRegistrationScreen :: RegistrationScreen
initRegistrationScreen = RegistrationScreen
  { stateRegistration = NoRegistrationState
  , newName = ""
  , newPassword = ""
  }

initRecordsScreen :: RecordsScreen
initRecordsScreen = RecordsScreen
  { stateRecords = NoRecordsState
  , users = []
  }

initSettingsScreen :: SettingsScreen
initSettingsScreen = SettingsScreen
  { stateSettings = NoSettingsState
  , gameType = PlayersGame
  }

----------------------   D R A W     S C R E E N S   ------------------------------
menuItemWidth :: Float   -- Ширина
menuItemWidth = 200.0

menuItemHeight :: Float  -- Высота
menuItemHeight = 50.0

strScaleWidth :: Float
strScaleWidth = 0.20

strScaleHeight :: Float
strScaleHeight = 0.15

loginHeight :: Float
loginHeight = 150

passwordHeight :: Float
passwordHeight = 40

nameLength :: Float
nameLength = 10

passwordLength :: Float
passwordLength = 10

enterHeight :: Float
enterHeight = -80

registrationHeight :: Float
registrationHeight = -150

newGameHeight :: Float
newGameHeight = 130

recordsHeight :: Float
recordsHeight = 60

settingsHeight :: Float
settingsHeight = -10

exitHeight :: Float
exitHeight = -200

headerHeight :: Float
headerHeight = 190

recordsIndent :: Float
recordsIndent = -220

firstRecordHeight :: Float
firstRecordHeight = 150

gameTypeSettingsHeight :: Float
gameTypeSettingsHeight = 150

gameTypeStrX :: Float
gameTypeStrX = -200

gameTypeVarX :: Float
gameTypeVarX = -10

buttonColor :: Color
buttonColor = makeColorI 220 220 190 255 
  --  245 245 220 255 Цвет фона

drawFirstScreen :: FirstScreen -> Picture
drawFirstScreen screen = pictures [drawLogin (name screen)
                                  , drawPassword (password screen)
                                  , drawEnter
                                  , drawRegistration
                                  , firstStateBorder (state1 screen)
                                  , allocateBorder1 (state1 screen)
                                  ]

drawSecondScreen :: SecondScreen -> Picture
drawSecondScreen screen = pictures [drawNewGame
                                   , drawRecords
                                   , drawSettings
                                   , allocateBorder2 (state2 screen)
                                   ]

firstStateBorder :: FirstScreenState -> Picture
firstStateBorder state | state == Login = translate (-menuItemWidth / 2) loginHeight drawMenuBorder
                  | state == Password = translate (-menuItemWidth / 2) passwordHeight drawMenuBorder
                  | otherwise = blank

               -- Прямоугольник по координатам
drawItem :: Float -> Float -> Color -> Picture
drawItem x0 y0 myColor = picture
   where
      picture = color myColor figure
      figure = polygon [(x0, y0), (x0 + menuItemWidth, y0), 
                        (x0 + menuItemWidth, y0 + menuItemHeight), (x0, y0 + menuItemHeight)]


               -- Прямоугольник - пункт меню
drawMenuItem :: Float -> Color -> Picture
drawMenuItem height myColor = translate (-menuItemWidth / 2) height picture
   where
      picture = color myColor figure
      figure = polygon [(0.0, 0.0), (menuItemWidth, 0.0), 
                        (menuItemWidth, menuItemHeight), (0.0, menuItemHeight)]

               -- Окно ввода логина
drawLogin :: Name -> Picture
drawLogin name = pictures [(drawMenuItem loginHeight white)
                          , writeMenuStr (fromIntegral (length name)) height name
                          , writeMenuStr 5 (loginHeight - 100*strScaleHeight) "Login"
                          ]
   where
      height = loginHeight + menuItemHeight / 2.5

               -- Окно ввода пароля
drawPassword :: Password -> Picture
drawPassword password = pictures [(drawMenuItem passwordHeight white)
                                 , writeMenuStr (fromIntegral (length password)) height password
                                 , writeMenuStr 8 (passwordHeight - 100*strScaleHeight) "Password"
                                 ]
   where
      height = passwordHeight + menuItemHeight / 2.5

               -- Вход
drawEnter :: Picture
drawEnter = pictures [(drawMenuItem enterHeight buttonColor)
                     , writeMenuStr 5 height "Enter"
                     ]
   where
      height = enterHeight + menuItemHeight / 2.5

               -- Регистрация
drawRegistration :: Picture
drawRegistration = pictures [(drawMenuItem registrationHeight buttonColor)
                            , writeMenuStr 12 height "Registration"
                            ]
   where
      height = registrationHeight + menuItemHeight / 2.5

               -- Новая игра
drawNewGame :: Picture
drawNewGame = pictures [(drawMenuItem newGameHeight buttonColor)
                       , writeMenuStr 8 height "New game"
                       ]
   where
      height = newGameHeight + menuItemHeight / 2.5

               -- Рекорды
drawRecords :: Picture
drawRecords = pictures [(drawMenuItem recordsHeight buttonColor)
                       , writeMenuStr 7 height "Records"
                       ]
   where
      height = recordsHeight + menuItemHeight / 2.5

               -- Настройки
drawSettings :: Picture
drawSettings = pictures [(drawMenuItem settingsHeight buttonColor)
                        , writeMenuStr 8 height "Settings"
                        ]
   where
      height = settingsHeight + menuItemHeight / 2.5

               -- Подпись меню
writeMenuStr :: Float -> Float -> String -> Picture
writeMenuStr letters height str = translate x height (scale strScaleWidth strScaleHeight picture)
   where
      picture = color black (text str)
      x = -letters * 13 / 2 

               -- Подпись
writeStr :: Float -> Float -> String -> Picture
writeStr x y str = translate x y (scale strScaleWidth strScaleHeight picture)
   where
      picture = color black (text str)


drawMenuBorder :: Picture
drawMenuBorder = color black (line [(0.0, 0.0), (menuItemWidth, 0.0), 
                                     (menuItemWidth, menuItemHeight), (0.0, menuItemHeight), (0.0, 0.0)])

allocateBorder1 :: FirstScreenState -> Picture
allocateBorder1 Enter = translate (-menuItemWidth / 2) enterHeight drawMenuBorder
allocateBorder1 EnterBorder = translate (-menuItemWidth / 2) enterHeight drawMenuBorder
allocateBorder1 RegistrationBorder = translate (-menuItemWidth / 2) registrationHeight drawMenuBorder
allocateBorder1 LoginBorder = translate (-menuItemWidth / 2) loginHeight drawMenuBorder
allocateBorder1 PasswordBorder = translate (-menuItemWidth / 2) passwordHeight drawMenuBorder
allocateBorder1 _ = blank

allocateBorder2 :: SecondScreenState -> Picture
allocateBorder2 NewGame = translate (-menuItemWidth / 2) newGameHeight drawMenuBorder
allocateBorder2 NewGameBorder = translate (-menuItemWidth / 2) newGameHeight drawMenuBorder
allocateBorder2 RecordsBorder = translate (-menuItemWidth / 2) recordsHeight drawMenuBorder
allocateBorder2 SettingsBorder = translate (-menuItemWidth / 2) settingsHeight drawMenuBorder
allocateBorder2 _ = blank


-----------------------------  D R A W   R E G I S T R A T I O N  --------------------------------

drawRegistrationScreen :: RegistrationScreen -> Picture
drawRegistrationScreen screen = pictures [drawLogin (newName screen)
                                         , drawPassword (newPassword screen)
                                         , drawEnter
                                         , drawExit
                                         , registrationStateBorder (stateRegistration screen)
                                         , allocateBorderRegistration (stateRegistration screen)
                                         ]

registrationStateBorder :: RegistrationScreenState -> Picture
registrationStateBorder state | state == LoginRegistration = translate (-menuItemWidth / 2) loginHeight drawMenuBorder
                              | state == PasswordRegistration = translate (-menuItemWidth / 2) passwordHeight drawMenuBorder
                              | otherwise = blank

allocateBorderRegistration :: RegistrationScreenState -> Picture
allocateBorderRegistration EnterRegistration = translate (-menuItemWidth / 2) enterHeight drawMenuBorder
allocateBorderRegistration EnterRegistrationBorder = translate (-menuItemWidth / 2) enterHeight drawMenuBorder
allocateBorderRegistration LoginRegistrationBorder = translate (-menuItemWidth / 2) loginHeight drawMenuBorder
allocateBorderRegistration PasswordRegistrationBorder = translate (-menuItemWidth / 2) passwordHeight drawMenuBorder
allocateBorderRegistration _ = blank


-----------------------------  D R A W   R E C O R D S  --------------------------------

drawRecordsScreen :: RecordsScreen -> Picture
drawRecordsScreen screen = pictures [ drawHeader "Records"
                                    , drawExit
                                    , writeRecords 
                                    , allocateBorderRecords (stateRecords screen)
                                    ]

allocateBorderRecords :: RecordsScreenState -> Picture
allocateBorderRecords ExitRecordsBorder = translate (-menuItemWidth / 2) exitHeight drawMenuBorder
allocateBorderRecords _ = blank

writeRecords :: Picture
writeRecords = writeListRecords initUsers 0 -- queryTableRec

initUsers :: Users
initUsers = initUser1 : initUser2 : initUser1 : initUser3 : initUser4 : initUser2 : []

initUser1 :: User
initUser1 = ("abc", "15")

initUser2 :: User
initUser2 = ("1234567890", "1234567890")

initUser3 :: User
initUser3 = ("defs", "1df5")

initUser4 :: User
initUser4 = ("1234jh", "4567890")

writeListRecords :: Users -> Float -> Picture
writeListRecords [] _ = blank
writeListRecords ((name1, password1):xs) num | num < 5 = pictures [ pictureName1 
                                                                  , pictureName2
                                                                  , pictureScore 
                                                                  , writeListRecords xs (num + 1)
                                                                  ]
                                             | otherwise = blank
   where
      pictureName1 = translate recordsIndent y scalePictureName
      pictureName2 = translate (recordsIndent + nameLength*16) y scalePictureName
      pictureScore = translate (recordsIndent + nameLength*32) y scalePictureScore
      scalePictureName = (scale strScaleWidth strScaleHeight (color black (text name1)))
      scalePictureScore = (scale strScaleWidth strScaleHeight (color black (text password1)))
      y = firstRecordHeight - 50*num


-----------------------------  D R A W   S E T T I N G S  --------------------------------

drawSettingsScreen :: SettingsScreen -> Picture
drawSettingsScreen screen = pictures [ drawHeader "Settings"
                                     , drawExit
                                     , writeGameType
                                     , gameTypeSelectedBorder (gameType screen)
                                     , allocateBorderSettings (stateSettings screen)
                                     ]

allocateBorderSettings :: SettingsScreenState -> Picture
allocateBorderSettings ExitSettingsBorder = translate (-menuItemWidth / 2) exitHeight drawMenuBorder
allocateBorderSettings PlayersGameBorder = translate x y1 drawMenuBorder
   where
      x = gameTypeVarX - 10
      y1 = gameTypeSettingsHeight - 20
allocateBorderSettings AIGameBorder = translate x y2 drawMenuBorder
   where
      x = gameTypeVarX - 10
      y2 = gameTypeSettingsHeight - menuItemHeight - 30
allocateBorderSettings _ = blank


gameTypeSelectedBorder :: GameType -> Picture
gameTypeSelectedBorder PlayersGame = translate x y1 drawMenuBorder
   where
      x = gameTypeVarX - 10
      y1 = gameTypeSettingsHeight - 20
gameTypeSelectedBorder AIGame = translate x y2 drawMenuBorder
   where
      x = gameTypeVarX - 10
      y2 = gameTypeSettingsHeight - menuItemHeight - 30


writeGameType :: Picture
writeGameType = pictures [ drawItem (gameTypeVarX - 10) (gameTypeSettingsHeight - 20) buttonColor
                         , drawItem (gameTypeVarX - 10) (gameTypeSettingsHeight - menuItemHeight - 30) buttonColor
                         , writeStr gameTypeStrX gameTypeSettingsHeight "Game type"
                         , writeStr gameTypeVarX gameTypeSettingsHeight "Two players"
                         , writeStr gameTypeVarX (gameTypeSettingsHeight - 60) "AI"
                         ]

               -- Заголовок
drawHeader :: String -> Picture
drawHeader str = writeMenuStr (fromIntegral (length str)) height str
   where
      height = headerHeight + menuItemHeight / 2.5

               -- Выход
drawExit :: Picture
drawExit = pictures [(drawMenuItem exitHeight buttonColor)
                    , writeMenuStr 4 height "Exit"
                    ]
   where
      height = exitHeight + menuItemHeight / 2.5

------------------------------------------------------------------------------------------

handleFirstScreen :: Event -> FirstScreen -> FirstScreen
handleFirstScreen (EventKey (MouseButton LeftButton) _ _ mouse) screen = changeFirstScreen mouse True screen
handleFirstScreen (EventMotion mouse) screen = changeFirstScreen mouse False screen
handleFirstScreen _ screen = screen

changeFirstScreen :: Point -> Bool -> FirstScreen -> FirstScreen
changeFirstScreen point False screen | (state1 screen) == Login = FirstScreen { state1 = Login
                                                                              , name = (name screen)
                                                                          --    , name = do {s <- getLine
                                                                            --               return s} 
                                                                    --          , name = readName
                                                                              , password = (password screen)
                                                                              }
                                     | (state1 screen) == Password = FirstScreen { state1 = Password
                                                                                 , name = (name screen)
                                                                                 , password = (password screen)
                                                                                 }
                                     | otherwise = FirstScreen { state1 = changeFirstState point False
                                                               , name = (name screen)
                                                               , password = (password screen)
                                                               }
changeFirstScreen point True screen = FirstScreen { state1 = changeFirstState point True
                                                  , name = (name screen)
                                                  , password = (password screen)
                                                  }

changeFirstState :: Point -> Bool -> FirstScreenState
changeFirstState point True | isButtonMenu point enterHeight == True = Enter
                            | isButtonMenu point loginHeight == True = Login
                            | isButtonMenu point passwordHeight == True = Password
                            | isButtonMenu point registrationHeight == True = Registration
                            | otherwise = NoFirstState
changeFirstState point False | isButtonMenu point enterHeight == True = EnterBorder
                             | isButtonMenu point registrationHeight == True = RegistrationBorder
                             | isButtonMenu point loginHeight == True = LoginBorder
                             | isButtonMenu point passwordHeight == True = PasswordBorder
                             | otherwise = NoFirstState

--readName :: FirstScreen -> FirstScreen
--readName screen = do str <- getLine
--                     return FirstScreen { state1 = Login
--                              , name = str
--                              , password = (password screen)
--                              }

readName :: IO String
readName = do
   s <- getLine
   return s

--readName :: FirstScreen -> Name
--readName screen | (state1 screen) == Login = getName
--                | otherwise = (name screen)

handleRegistrationScreen :: Event -> RegistrationScreen -> RegistrationScreen
handleRegistrationScreen (EventKey (MouseButton LeftButton) _ _ mouse) screen = changeRegistrationScreen mouse True screen
handleRegistrationScreen (EventMotion mouse) screen = changeRegistrationScreen mouse False screen
handleRegistrationScreen _ screen = screen

changeRegistrationScreen :: Point -> Bool -> RegistrationScreen -> RegistrationScreen
changeRegistrationScreen point False screen 
       | (stateRegistration screen) == LoginRegistration = 
                          RegistrationScreen { stateRegistration = LoginRegistration
                                             , newName = (newName screen)
                                             , newPassword = (newPassword screen)
                                             }
       | (stateRegistration screen) == PasswordRegistration = 
                          RegistrationScreen { stateRegistration = PasswordRegistration
                                             , newName = (newName screen)
                                             , newPassword = (newPassword screen)
                                             }
       | otherwise = RegistrationScreen { stateRegistration = changeRegistrationState point False
                                        , newName = (newName screen)
                                        , newPassword = (newPassword screen)
                                        }
changeRegistrationScreen point True screen = 
                          RegistrationScreen { stateRegistration = changeRegistrationState point True
                                             , newName = (newName screen)
                                             , newPassword = (newPassword screen)
                                             }

changeRegistrationState :: Point -> Bool -> RegistrationScreenState
changeRegistrationState point True | isButtonMenu point enterHeight == True = EnterRegistration
                                   | isButtonMenu point loginHeight == True = LoginRegistration
                                   | isButtonMenu point passwordHeight == True = PasswordRegistration
                                   | isButtonMenu point exitHeight == True = ExitRegistration
                                   | otherwise = NoRegistrationState
changeRegistrationState point False | isButtonMenu point enterHeight == True = EnterRegistrationBorder
                                    | isButtonMenu point loginHeight == True = LoginRegistrationBorder
                                    | isButtonMenu point passwordHeight == True = PasswordRegistrationBorder
                                    | otherwise = NoRegistrationState



handleSecondScreen :: Event -> SecondScreen -> SecondScreen
handleSecondScreen (EventKey (MouseButton LeftButton) _ _ mouse) _ = changeSecondScreen mouse True
handleSecondScreen (EventMotion mouse) _ = changeSecondScreen mouse False
handleSecondScreen _ screen = screen

changeSecondScreen :: Point -> Bool -> SecondScreen
changeSecondScreen point fl = SecondScreen
  { state2 = changeSecondState point fl
  --, tryUser = ("", "")
  }

changeSecondState :: Point -> Bool -> SecondScreenState
changeSecondState point True | isButtonMenu point newGameHeight == True = NewGame
                             | isButtonMenu point recordsHeight == True = Records
                             | isButtonMenu point settingsHeight == True = Settings
                             | otherwise = NoSecondState
changeSecondState point False | isButtonMenu point newGameHeight == True = NewGameBorder
                              | isButtonMenu point recordsHeight == True = RecordsBorder
                              | isButtonMenu point settingsHeight == True = SettingsBorder
                              | otherwise = NoSecondState


handleRecordsScreen :: Event -> RecordsScreen -> RecordsScreen
handleRecordsScreen (EventKey (MouseButton LeftButton) _ _ mouse) screen = changeRecordsScreen mouse True screen
handleRecordsScreen (EventMotion mouse) screen = changeRecordsScreen mouse False screen
handleRecordsScreen _ screen = screen

changeRecordsScreen :: Point -> Bool -> RecordsScreen -> RecordsScreen
changeRecordsScreen point fl screen = RecordsScreen
  { stateRecords = changeRecordsState point fl
  --, tryUser = ("", "")
  , users = (users screen)
  }

changeRecordsState :: Point -> Bool -> RecordsScreenState
changeRecordsState point True | isButtonMenu point exitHeight == True = ExitRecords
                              | otherwise = NoRecordsState
changeRecordsState point False | isButtonMenu point exitHeight == True = ExitRecordsBorder
                               | otherwise = NoRecordsState


handleSettingsScreen :: Event -> SettingsScreen -> SettingsScreen
handleSettingsScreen (EventKey (MouseButton LeftButton) _ _ mouse) screen = changeSettingsScreen mouse True screen
handleSettingsScreen (EventMotion mouse) screen = changeSettingsScreen mouse False screen
handleSettingsScreen _ screen = screen

changeSettingsScreen :: Point -> Bool -> SettingsScreen -> SettingsScreen
changeSettingsScreen point fl screen = SettingsScreen
  { stateSettings = changeSettingsState point fl
  , gameType = changeGameType point fl screen --(gameType screen)
  }

changeGameType :: Point -> Bool -> SettingsScreen -> GameType
changeGameType _ False screen = (gameType screen) 
changeGameType point True screen | isButton point x0 yPlayers0 x1 yPlayers1 == True = PlayersGame
                                 | isButton point x0 yAI0 x1 yAI1 == True = AIGame 
                                 | otherwise = (gameType screen)
   where
      x0 = gameTypeVarX - 10
      x1 = x0 + menuItemWidth
      yPlayers0 = gameTypeSettingsHeight - 20
      yPlayers1 = yPlayers0 + menuItemHeight
      yAI0 = gameTypeSettingsHeight - menuItemHeight - 30
      yAI1 = yAI0 + menuItemHeight

changeSettingsState :: Point -> Bool -> SettingsScreenState
changeSettingsState point True | isButtonMenu point exitHeight == True = ExitSettings
                               | otherwise = NoSettingsState
changeSettingsState point False | isButtonMenu point exitHeight == True = ExitSettingsBorder
                                | isButton point x0 yPlayers0 x1 yPlayers1 == True = PlayersGameBorder
                                | isButton point x0 yAI0 x1 yAI1 == True = AIGameBorder 
                                | otherwise = NoSettingsState
   where
      x0 = gameTypeVarX - 10
      x1 = x0 + menuItemWidth
      yPlayers0 = gameTypeSettingsHeight - 20
      yPlayers1 = yPlayers0 + menuItemHeight
      yAI0 = gameTypeSettingsHeight - menuItemHeight - 30
      yAI1 = yAI0 + menuItemHeight

             -- Принадлежит ли точка заданной прямоугольной области
isButton :: Point -> Float -> Float -> Float -> Float -> Bool
isButton (x, y) x0 y0 x1 y1 = (x >= x0) && 
                         (x <= x1) &&
                         (y >= y0) &&
                         (y <= y1)


             -- Принадлежит ли точка клавише меню, заданной вторым параметром - высотой кнопки
isButtonMenu :: Point -> Float -> Bool
isButtonMenu (x, y) height = (x >= -menuItemWidth / 2) && 
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
  , gameScore = amountScores (gameBoard game)
  , gameWinner = winner game

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
