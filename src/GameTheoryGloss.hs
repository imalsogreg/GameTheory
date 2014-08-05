{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Main where

import Control.Monad
import Data.Map
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.FilePath

main :: IO ()
main = do
  s0 <- state0
  playIO (InWindow "Game Theory" (500,500) (10,10))
    white 30 s0 worldPic input timestep

state0 :: IO World
state0 = do
  let picNames = ["qMark.bmp", "coors.bmp", "sam.bmp", "leffe.bmp"]
  pics <- forM picNames $ \n ->
    return . (n,) =<< loadBMP ("pics" </> n)
  return $ World 640 480 (fromList pics) WaitingBoth

worldPic :: World -> IO Picture
worldPic w@World{..} = return $ pictures [
    line [(0,-height/2),(0,height/2)]
  , statePic w
  ]

statePic :: World -> Picture
statePic (World w h r WaitingBoth) =
  

input :: Event -> World -> IO World
input e w = return w

timestep :: Float -> World -> IO World
timestep t w = return w

data World = World {
    width     :: Float
  , height    :: Float
  , resources :: Map String Picture
  , gameState :: WorldState
  }

data WorldState = WaitingBoth
                | ConfirmReset
                | GotOne  Player
                | GotBoth (Result, Result)

data Player = PlayerLeft
            | PlayerRight

data Result = Coors
            | Sam
            | Leffe