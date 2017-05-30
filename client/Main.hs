{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)

import Graphics.Gloss.Interface.IO.Game
import Network.WebSockets
import System.Exit (exitSuccess)

import Config
import Draw
import Game
import Models



-- | Состояние игры на клиенте
data GameState = GameState
  { gameGame    :: TVar Game  -- Состояние доски 
  , gameConnection  :: Connection       --  Сокет 'Connection' с сервером
  }


main :: IO ()
main = do
  runIO "localhost" 8000 



drawGames :: GameState -> IO Picture
drawGames  GameState{..} = do
  g <- readTVarIO gameGame
  return $ drawGame g   



-- | Обработка обновлений с сервера
handleUpdates :: GameState -> IO ()
handleUpdates GameState{..} = forever $ do
  game <- receiveData gameConnection
  atomically $ writeTVar gameGame game


-- | Обновить состояние игровой вселенной (клиент)
updateGames :: Float -> GameState -> IO GameState
updateGames _ g = return g

handleGames :: Event -> GameState -> IO GameState
handleGames (EventKey (MouseButton LeftButton) Up _ mouse) g@GameState{..}  = do
  g1 <- readTVarIO gameGame
  _ <- forkIO $ sendBinaryData gameConnection (placeStone (mouseToCell mouse) g1)
  return g

handleGames (EventMotion mouse) g@GameState{..}  = do
   g1 <- readTVarIO gameGame
   _ <- forkIO $ sendBinaryData gameConnection (placeShadowStone  (mouseToCell mouse) g1)
   return g

handleGames (EventKey (SpecialKey KeySpace) Up _ _) g@GameState{..} = do
   g1 <- readTVarIO gameGame
   _ <- forkIO $ sendBinaryData gameConnection (takePass (setPass g1))
   return g

handleGames _ g = return g

runIO :: String -> Int ->  IO ()
runIO ip port  = do
  game <- atomically $ newTVar initGame
  runClient ip port "/connect" $ \conn -> do
    let gs = GameState game  conn
    _ <- forkIO (handleUpdates gs)
    playIO display bgColor fps gs drawGames handleGames updateGames 
  where
    display = InWindow "Game Go" (screenWidth, screenHeight) (200, 200)
    bgColor = makeColorI 245 245 220 255 
    fps     = 60      
