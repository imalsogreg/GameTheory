module UISetup where

import           Control.Lens                hiding ((#), set)
import           Control.Monad
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import           System.FilePath

import           Types
import           Paths_GameTheory



------------------------------------------------------------------------------
setupHelpScreen :: UI Element
setupHelpScreen = do
  static <- liftIO $ (</> "wwwroot") `liftM` getDataDir
  [lURL,mURL,sURL] <- mapM (loadFile "image/png" . (static </>))
                            ["largeReward.png"
                            ,"mediumReward.png"
                            ,"smallReward.png"]
  let youCText = UI.h3 # set UI.id_ "helpYouCoop"
  let youDText = UI.h3 # set UI.id_ "helpYouDefect"
  let themCText = UI.h3 # set UI.id_ "helpTheyCoop"
  let themDText = UI.h3 # set UI.id_ "helpTheyDefect"
  let l = UI.img # set UI.src lURL
  let m = UI.img # set UI.src mURL
  let s = UI.img # set UI.src sURL
  UI.new #+ [grid
             [ [UI.new,    youCText,         youDText  ]
             , [themCText, smallGridBox m m, smallGridBox l s]
             , [themDText, smallGridBox s l, smallGridBox s s]
             ]]

  where
    smallGridBox :: UI Element -> UI Element -> UI Element
    smallGridBox youGet theyGet =
      column [
          row [UI.p # set html "You get:",  youGet  ]
        , row [UI.p # set html "They get:", theyGet ]
        ]


------------------------------------------------------------------------------
setupButtonElements :: String -> UI Element
setupButtonElements groupName = do
  UI.mkElement "groupName" #+ [column [
      UI.h4 # set UI.id_ (groupName ++ "TopText")
    , row [ column [ UI.img # set UI.id_ (groupName ++ "ABtn")
                   , UI.img # set UI.id_ (groupName ++ "AText")]
          , column [ UI.img # set UI.id_ (groupName ++ "BBtn")
                   , UI.img # set UI.id_ (groupName ++ "BText")]
          ]
    ]]

  
------------------------------------------------------------------------------
setupDilemmaView :: UI Element
setupDilemmaView =
  UI.new #+ [column [
    row [ column [UI.img # set UI.id_ "themeUp"
                 ,UI.img # set UI.id_ "themeDown"]
          , UI.h1  # set UI.id_ "themeText"]
    , row [ setupButtonElements "buttonsLeft"
          , setupButtonElements "buttonsRight" ]
    , UI.h2 # set UI.id_ "instructions"
    , UI.button # set UI.id_ "helpButton" #+ [string "?"]
    , setupHelpScreen
    ]]

  