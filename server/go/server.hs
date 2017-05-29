{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Go.Server where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (catch)
import Control.Monad (forever)
import Control.Monad.Random (evalRand, newStdGen)
import Data.Map (Map)
import qualified Data.Map as Map

import Network.HTTP.Types (status400)
import Network.Wai (responseLBS)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
import Servant

import Config
import Draw
import Game
import Models

-- | Соединение с клиентом
type Client = Connection 

-- | Конфигурация сервера 
data Config = Config
{
    configGame :: TVar Game -- игровое поле
   , configClients :: TVar (Map PlayerName Client) --список подключенных клиентов с уникальным именем
   , configName :: TVar [PlayerNames] --список свободных имен

}


-- | Конфигурация по умолчанию. Пустое поле, нет клиентов.
defaultConfig :: IO Config
defaultConfig do =
	cfg <- atomically $ Config
		<$> newTVar initGame
		<*> newTvar Map.empty
		<*> newTVar (map show [1..])
return cfg

-- | API
type GoAPI = "connect" :> Raw

server :: Config -> Server GoAPI
server config  = websocketsOr defaultConnectionOptions wsApp backupApp
where 
	wsApp :: ServerApp
	wsApp pending_con = do
		conn <- AcceptRequest pending_con
		name <- addClient conn config
		putStrLn $ name ++ "joined!"
		hendleActions name conn config

	-- | Используется для не сокетных соединений 

	backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"


-- | Добавление клиента на сервер на сервер
addClient :: Client -> Config -> IO PlayerName
addClient client Config{..} = do
  g <- newStdGen
  atomically $ do
    name:names <- readTVar configNames
    writeTVar configName names
    modifyTVar configClients (Map.insert name client)
    return name

-- | Обработка событий с клиентов
handleActions :: PlayerName -> Connection -> Config -> IO ()
handleActions name conn cfg@Config{..} = forever $ do
  action <- receiveData conn
  atomically $ do
    modifyTVar configGame (handleGame action)

-- | Обновление доски

periodicUpdates :: Int -> Config -> IO ()
periodicUpdates ms cfg@Config{..} = forever $ do
  threadDelay ms -- wait ms milliseconds
  g <- newStdGen
  game <- atomically $ do
    game <- (flip evalRand g . updateSreen secs) <$> readTVar configGame
    writeTVar configGame game
    return game
  broadcastUpdate game cfg
  where
    secs = fromIntegral ms / 1000000

-- | Отправка изменений на клиенты
broadcastUpdate :: Game -> Config -> IO ()
broadcastUpdate game cfg@Config{..} = do
  clients <- readTVarIO configClients
  mapM_ (forkIO . sendUpdate) (Map.toList clients)
  where
    sendUpdate (name, conn) = sendBinaryData conn game `catch` handleClosedConnection name


-- | Обработка отключения клиента
 handleClosedConnection :: PlayerName -> ConnectionException -> IO ()
    handleClosedConnection name _ = do
      putStrLn (name ++ " disconnected.")
      atomically $ do
        modifyTVar configClients  (Map.delete name)
        modifyTVar configGame (kickPlayer name)

kickPlayer :: PlayerName -> Game -> Game 
kickPlayer name g = initGame


