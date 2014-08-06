module Render where

import           Control.Applicative
import           Control.Lens hiding ((#),element,set)
import           Control.Monad
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

import           Types


------------------------------------------------------------------------------
renderDilemmaView :: GameState -> Window -> DilemmaElements -> UI ()
renderDilemmaView gs w de = do
  set text (show (gs ^. gameTheme) ++ " Dilemma 2")
    (pure $ de^.themeText)
  
  return ()

--toggleHelp :: 
