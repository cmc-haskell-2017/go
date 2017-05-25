{-# LANGUAGE OverloadedStrings #-}
module Database where

import           Control.Applicative
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Control.Monad

type NamePlayer = T.Text

type ScoreOnePlayer = Float

type Id_type = Int

type Password = T.Text

-- | Структура таблицы в базе данных для рекордов
data Records = Records {id_record   :: Id_type,
                        name1       :: NamePlayer,
                        name2       :: NamePlayer,
                        score1      :: ScoreOnePlayer,
                        score2      :: ScoreOnePlayer} deriving (Show)

-- | Структура строки-результата запроса SQL для списка рекордов
data Record = Record {namePlayer1   :: NamePlayer,
                      namePlayer2   :: NamePlayer,
                      scorePlayer1  :: ScoreOnePlayer,
                      scorePlayer2  :: ScoreOnePlayer} deriving (Show)

-- | Структура строки-результата запроса SQL для старых рекордов (нужны для сравнения)
data ScoreValueRow = ScoreValueRow {oldScore1 :: ScoreOnePlayer,
                                    oldScore2 :: ScoreOnePlayer} deriving (Show)
-- | Структура таблицы в базе данных для пользователя
data Users = Users {id_user   :: Id_type,
                    name      :: NamePlayer,
                    password  :: Password} deriving (Show)

instance ToRow Records where
  toRow (Records id_record name1 name2 score1 score2) = toRow (id_record, name1, name2, score1, score2)

instance ToRow Users where
  toRow (Users id_record name password) = toRow (id_record, name, password)

instance FromRow Record where
  fromRow = Record <$> field <*> field <*> field <*> field

instance FromRow ScoreValueRow where
  fromRow = ScoreValueRow <$> field <*> field

-- | Функция-интерфейс для взамодействия с БД (интерфейс командной строки)
manageBD :: IO()
manageBD = do
  putStrLn "\nInput\n1 - for add record;\n2 - print list of records;\n3 - for registration;\n4 - for authorization;\n5 - for exit"
  manage <- getLine
  subManage manage

-- | Функция непосредственно вызывающая обработчики для различных событий (для интерфейса)
subManage :: [Char] -> IO()
subManage manage | manage == "1" = do
                        addRecord
                        manageBD
                 | manage == "2" = do
                        printRecord
                        manageBD
                 | manage == "3" = do
                        registrationUser
                        manageBD
                 | manage == "4" = do
                        authorizationUser
                        manageBD
                 | manage == "5" = print "Bye."

-- | Добавление нового рекорда (интерфейс)
addRecord :: IO()
addRecord = do
  createTableRecord
  putStrLn "enter name first player "
  name1 <- getLine
  putStrLn "enter name second player "
  name2 <- getLine
  putStrLn "enter score first player "
  score1 <- getLine
  putStrLn "enter score second player "
  score2 <- getLine
  updateTableRec (name1, name2, charListToFloat(score1), charListToFloat(score2))

-- | Создание таблицы рекордов, если таблица отсутствует
createTableRecord :: IO ()
createTableRecord = do
  conn <- open "game.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS records (id_record INTEGER, name1 TEXT, name2 TEXT, score1 REAL, score2 REAL)"
  close conn

-- | Функция обновления таблицы рекордов
updateTableRec :: (String, String, ScoreOnePlayer, ScoreOnePlayer) -> IO ()
updateTableRec (name1, name2, score1, score2)  = do
  printRecord
  conn <- open "game.db"
  id_record <- queryNamed conn "SELECT id_record from records where name1 = :name1 AND name2 = :name2" [":name1" := (T.pack name1), ":name2" := (T.pack name2)] :: IO [Only (Maybe Id_type)]
  id_record2 <- queryNamed conn "SELECT id_record from records where name1 = :name2 AND name2 = :name1" [":name1" := (T.pack name1), ":name2" := (T.pack name2)] :: IO [Only (Maybe Id_type)]
  real_action_updateTableRec (getMaybeFromRow id_record) (getMaybeFromRow id_record2) (name1, name2, score1, score2)
  close conn

-- | Вспомогательная функция для обновления рекордов (добавляет новый рекорд,
-- | если пользователи с такими именами еще не играли, если же играли, то
-- | то обновление происходит только в случае если оба игрока набрали больше
-- | очков, чем в прошлый раз)
real_action_updateTableRec :: Maybe Id_type -> Maybe Id_type -> (String, String, ScoreOnePlayer, ScoreOnePlayer) -> IO ()
real_action_updateTableRec id_record id_record2 (name1, name2, score1, score2)
  | id_record == Nothing && id_record2 == Nothing= do -- | В таблице records нет записи рекорда для данных игроков
        conn <- open "game.db"
        max <- query_ conn "SELECT MAX(id_record) from records" :: IO [Only (Maybe Id_type)]
        execute conn "INSERT INTO records (id_record, name1, name2, score1, score2) VALUES (?, ?, ?, ?, ?)" (Records ((getId_typeNumber max) + 1) (T.pack name1) (T.pack name2) score1 score2)
        close conn
  | id_record /= Nothing = do -- | В таблице records есть запись рекорда для данных игроков причем игроки в том же порядке
        conn <- open "game.db"
        old_score <- queryNamed conn "SELECT score1, score2 from records WHERE id_record = :id_record" [":id_record" := id_record] :: IO [ScoreValueRow]
        updateRecordIfNesessary old_score score1 score2 (getJustFromMaybe id_record)
        close conn
  | otherwise = do -- | В таблице records есть запись рекорда для данных игроков, но игроки переставлены в записи
        conn <- open "game.db"
        old_score <- queryNamed conn "SELECT score1, score2 from records WHERE id_record = :id_record" [":id_record" := id_record2] :: IO [ScoreValueRow]
        updateRecordIfNesessary old_score score2 score1 (getJustFromMaybe id_record2)
        close conn

-- | Еще одна вспомогательная функция для обновления таблицы рекордов
-- | Она именно обновляет показатели рекордов для игроков (в случае, если они набрали больше очков)
updateRecordIfNesessary :: [ScoreValueRow] -> Float -> Float -> Id_type -> IO()
updateRecordIfNesessary old_score score1 score2 id_record
  | cmpScore old_score score1 score2 = do
      conn <- open "game.db"
      executeNamed conn "UPDATE records SET score1 = :score1, score2 = :score2 WHERE id_record = :id_record" [":score1" := score1, ":score2" := score2, ":id_record" := id_record]
      close conn
  | otherwise = do
      conn <- open "game.db"
      close conn

-- | Сравнение новых очков со старыми
cmpScore :: [ScoreValueRow] -> Float -> Float -> Bool
cmpScore ((ScoreValueRow oldScore1 oldScore2) : xs) score1 score2
              | score1 >= oldScore1 && score2 >= oldScore2 = True
              | otherwise = False

-- | Печать списка рекордов, после представления списка в спец. виде
printRecord :: IO ()
printRecord = do
      list <- queryTableRec
      putStrLn "\n\n Table of records"
      printOne (oneRecord(list))

-- | Данная функция делает список из кортежей вида [(имя_игрока_1, имя_игрока_2, количество_очков_игрока_1, количество_очков_игрока_2)]
oneRecord :: [Record] -> [(T.Text,T.Text,ScoreOnePlayer,ScoreOnePlayer)]
oneRecord ((Record n1 n2 s1 s2) : []) = [(n1,n2,s1,s2)]
oneRecord ((Record n1 n2 s1 s2) : xs) = (n1,n2,s1,s2) : (oneRecord (xs))

-- | Непосредственная печать списка рекордов в виде столбика из кортежей
printOne :: [(T.Text,T.Text,ScoreOnePlayer,ScoreOnePlayer)] -> IO()
printOne ((n1, n2, s1, s2) : []) = print (n1, n2, s1, s2)
printOne ((n1, n2, s1, s2) : xs) = do
  print (n1, n2, s1, s2)
  printOne xs

-- |  Выдает список рекордов (не в специальном виде, то есть еще нужно обработать для печати)
queryTableRec:: IO [Record]
queryTableRec = do
  conn <- open "game.db"
  list_name <- query_ conn "SELECT name1, name2, score1, score2 from records ORDER BY score1 DESC" :: IO [Record]
  close conn
  return list_name

-- | Взяли [Only (Maybe Id_type)], вернули Maybe Id_type
getMaybeFromRow:: [Only (Maybe Id_type)] -> Maybe Id_type
getMaybeFromRow [] = Nothing
getMaybeFromRow ((Only value) : xs) = value

-- | Взяли [Only (Maybe T.Text)], вернули Maybe Text
getMaybeFromRowText:: [Only (Maybe T.Text)] -> Maybe T.Text
getMaybeFromRowText [] = Nothing
getMaybeFromRowText ((Only value):xs) = value

-- | Берем Just из Maybe
getJustFromMaybe :: Maybe a -> a
getJustFromMaybe (Just value) = value

-- | От результата запроса возвращаем целочисленную числовую величину (это для id), 0 - если запись первая
getId_typeNumber :: [Only (Maybe Id_type)] -> Id_type
getId_typeNumber [] = 0
getId_typeNumber ((Only value):xs)|value == Nothing = 0
                                     |otherwise = (getJustFromMaybe value)

-- | Преобразует строку в вещественное число (нужно для добавления рекорда из интерфейса)
charListToFloat :: String -> Float
charListToFloat str = read str :: Float

-- | Регистрация нового пользователя (интерфейс)
registrationUser :: IO ()
registrationUser = do
  createTableUser
  putStrLn "\n"
  putStrLn "enter your nickname "
  name <- getLine
  putStrLn "enter password (not less then 4 symblols) "
  password <- getLine
  createUser (name, password)

-- | Создание таблицы пользователей, если таблица отсутствует
createTableUser :: IO ()
createTableUser = do
  conn <- open "game.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS users (id_record INTEGER, name TEXT, password TEXT)"
  close conn

-- | Непосредственное создание нового пользователя с проверкой ограничений
-- | на длину пароля и на уникальность ника пользователя
createUser :: (String, String) -> IO ()
createUser (name, password) | (length (password) > 3) = do
  conn <- open "game.db"
  old_name <- queryNamed conn "SELECT name from users WHERE name = :name" [":name" := name] :: IO [Only (Maybe T.Text)]
  if ((getMaybeFromRowText (old_name)) == Nothing)
  then do
    max <- query_ conn "SELECT MAX(id_record) from users" :: IO [Only (Maybe Id_type)]
    execute conn "INSERT INTO users (id_record, name, password) VALUES (?, ?, ?)" (Users ((getId_typeNumber max) + 1) (T.pack name) (T.pack password))
    close conn
  else do
    print "User with this name already exists. Please choose a different name."
    close conn
    registrationUser

-- | Авторизация пользователя (интерфейс)
authorizationUser :: IO String
authorizationUser = do
  putStrLn "\n"
  putStrLn "enter login "
  name <- getLine
  putStrLn "enter password "
  password <- getLine
  name <- authorizationTestUser (name, password)
  return name

-- | Непосредственная авторизация пользователя с проверкой пароля
authorizationTestUser :: (String, String) -> IO String
authorizationTestUser (name, password) | (length (password) > 3) = do
  conn <- open "game.db"
  password_table <- queryNamed conn "SELECT password from users WHERE name = :name" [":name" := name] :: IO [Only (Maybe T.Text)]
  if ((getMaybeFromRowText (password_table)) /= Nothing)
  then do
    if ((T.pack password) == (getJustFromMaybe(getMaybeFromRowText (password_table))))
    then do
      {-print ("Hi " ++ name)-}
      close conn
      pure name
    else do
      print "Wrong login or password. Please try again."
      close conn
      authorizationUser
  else do
      print "Wrong login or password. Please try again."
      close conn
      authorizationUser
