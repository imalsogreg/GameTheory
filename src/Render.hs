module Render where

import           Control.Lens hiding ((#),element,set)
import           Control.Monad
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

import           Types


------------------------------------------------------------------------------
renderDilemmaView :: GameState -> Window -> UI ()
renderDilemmaView gs w = do
  el <- getElementById w "themeText"
  maybe (UI.string "Missing element")
    ( (set text (show (gs ^. gameTheme) ++ " Dilemma")) . pure) el
  return ()