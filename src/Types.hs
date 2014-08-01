{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

data GameState = GameState {
    _gamePhase :: GamePhase
  , _gameTheme :: Theme
  }
    
data Theme = Prisoners | Academics | Spouses
           deriving (Eq, Ord, Show)

data Player = PlayerLeft
            | PlayerRight
            deriving (Eq, Show)

data GamePhase = Title
               | Waiting
               | LockedIn Player
               | RoundEnded
               | ShowHelp
               deriving (Eq, Show)

data DilemmaView = DilemmaView {
    _dilemmaViewElement :: Element
  , _leftButtons        :: ButtonElements
  , _rightButtons       :: ButtonElements
  , _instructions       :: Element
  , _helpButton         :: Element
  , _helpScreen         :: HelpScreen
  }

data ButtonElements = ButtonElements {
    _buttonElements :: Element
  , _buttonsTopText :: Element
  , _buttonAImage   :: Element
  , _buttonASubtext :: Element
  , _buttonBImage   :: Element
  , _buttonBSubtext :: Element
  }

data HelpScreen = HelpScreen {
    _helpScreenElement :: Element
  , _youCollabText     :: Element
  , _youDefectText     :: Element
  , _themCollabText    :: Element
  , _themDefectText    :: Element
  }
  
$(makeLenses ''GameState)
$(makeLenses ''DilemmaView)
$(makeLenses ''ButtonElements)
$(makeLenses ''HelpScreen)