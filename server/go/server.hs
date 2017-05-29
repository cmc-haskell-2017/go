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
