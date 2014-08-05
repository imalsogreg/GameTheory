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

data DilemmaElements = DilemmaElements {
    _themeText          :: Element
  , _themeUp            :: Element
  , _themeDown          :: Element
  , _leftButtons        :: ButtonElements
  , _rightButtons       :: ButtonElements
  , _instructions       :: Element
  , _helpButton         :: Element
  , _helpScreen         :: HelpScreen
  }

data ButtonElements = ButtonElements {
    _buttonsTopText :: Element
  , _buttonAImage   :: Element
  , _buttonASubtext :: Element
  , _buttonBImage   :: Element
  , _buttonBSubtext :: Element
  }

data HelpScreen = HelpScreen {
    _helpDiv           :: Element
  , _youCollabText     :: Element
  , _youDefectText     :: Element
  , _themCollabText    :: Element
  , _themDefectText    :: Element
  }
  
$(makeLenses ''GameState)
$(makeLenses ''DilemmaElements)
$(makeLenses ''ButtonElements)
$(makeLenses ''HelpScreen)