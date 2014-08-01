module Main where

import Prelude
import Control.Lens hiding ((#), element, set)
import Control.Monad (void, liftM)
import Control.Concurrent.STM
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import System.FilePath

import BBB
import Types
import Render
import UISetup
import Paths_GameTheory

main :: IO ()
main = do
  static <- (</> "wwwroot") `liftM` getDataDir
  let st = (GameState Title Prisoners)
  st' <- newTVarIO st
  startGUI defaultConfig {tpStatic = Just static} $ (setup st)


setup :: GameState -> Window -> UI ()
setup st rootWindow = void $ do
  userNameInput <- UI.input
                    # set (attr "placeholder") "User name"

  loginButton <- UI.button #+ [ string "Login" ]

  dv <- setupDilemmaView

  getBody rootWindow #+
    map element [ userNameInput, loginButton, dv ]

  renderDilemmaView st rootWindow



{-
main :: IO ()
main = playIO (InWindow "Game Theory" (500,500) (10,10))
       white 30 state0 worldPic input timestep

state0 = World test

worldPic :: World -> IO Picture
worldPic w = return $ circle 10

input e w = return w

timestep t w = return w

data World = World Int
-}