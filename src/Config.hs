module Config where

import Graphics.Gloss.Interface.Pure.Game

-- =========================================
-- Константы, параметры игры.
-- =========================================

-- | Радиус картинки камня.
radiusStone :: Float
radiusStone = 0.45

-- | Начальная фора(очки) белого игрока
-- может варьироваться уже по желания, но это позже в индивидуальных частях.
playerComi :: Float
playerComi = 0.5

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
