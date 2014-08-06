{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import qualified Data.Map as Map
import           Data.Maybe
import Data.Time
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.FilePath
import System.IO

main :: IO ()
main = do
  s0@World{..} <- state0
  playIO (InWindow "Game Theory" (floor width, floor height) (0,0))
    white 30 s0 worldPic input timestep

state0 :: IO World
state0 = do
  let picNames = ["qMark", "coors", "sam", "leffe", "btnsRequest"]
  pics <- forM picNames $ \n ->
    return . (n,) =<< loadBMP (("pics" </> n) ++ ".bmp")
--  mapM (setBufferMode NoBuffering) ["a.txt","b.txt","c.txt","d.txt"]
  return $ World 640 480 (Map.fromList pics) "a.txt" "b.txt" "c.txt" "d.txt" WaitingBoth


------------------------------------------------------------------------------
worldPic :: World -> IO Picture
worldPic w@World{..} = return $ pictures [
  statePic w
  ]

------------------------------------------------------------------------------
statePic :: World -> Picture
statePic wrl@World{..}
  | gameState == WaitingBoth = case Map.lookup "qMark" resources of
    Nothing -> error "Couldn't load qMark bitmap"
    Just q -> pictures [ picAt wrl PlayerLeft q
                       , picAt wrl PlayerRight q
                       , midLine wrl
                       ]
  | isConfirmReset gameState = text "Really abort trial? y/n"
  | gameState == RequestButtonsUp =
    case Map.lookup "btnsRequest" resources of
      Nothing -> error $ "Couldn't load btnsRequest" ++ concat (Map.keys resources)
      Just p  -> p
  | isGotOne gameState = case Map.lookup "qMark" resources of
    Nothing -> error "Couldn't load qMark bitmap"
    Just q  -> let (GotOne thePlayer theChoice) = gameState
                   remaining = otherPlayer thePlayer
               in pictures [ picAt wrl remaining q
                           , midLine wrl
                           ]
  | isGotBoth gameState = let (GotBoth l r) = gameState in
    case map (flip Map.lookup resources) ["coors","sam","leffe"] of
      [Just coors, Just sam, Just leffe] -> 
        case (l,r) of
          (Cooperate,Cooperate) -> pictures
            [ picAt wrl PlayerLeft sam
            , picAt wrl PlayerRight sam
            , midLine wrl ]
          (Cheat,Cheat) -> pictures
            [ picAt wrl PlayerLeft coors
            , picAt wrl PlayerRight coors
            , midLine wrl ]
          (Cheat,Cooperate) -> pictures
            [ picAt wrl PlayerLeft leffe
            , picAt wrl PlayerRight coors
            , midLine wrl ]
          (Cooperate,Cheat) -> pictures
            [ picAt wrl PlayerLeft coors
            , picAt wrl PlayerRight leffe
            , midLine wrl ]
      _ -> error "Couldn't load beer pictures."
    
  | otherwise = error "Impossible case in statePic"


------------------------------------------------------------------------------
picAt :: World -> Player -> Picture -> Picture
picAt World{..} p pic = translate x y pic
  where (x,y) = case p of
          PlayerLeft  -> (-width/3, 0)
          PlayerRight -> ( width/3, 0)


------------------------------------------------------------------------------
midLine :: World -> Picture
midLine World{..} = color black $ line [(0, (-2*height)/5)
                                       ,(0, (2*height)/5)
                                       ]


------------------------------------------------------------------------------
-- EventKey Key KeyState Modifiers (Float, Float)
input :: Event -> World -> IO World
input (EventMotion _)        w = return w
input (EventResize _)        w = return w
input (EventKey _ Down _ _ ) w = return w
input (EventKey k Up _ _) w@World{..}
  | isGotBoth gameState = do
    canGoAhead <- allBtnsOff w
    case canGoAhead of
      True  -> return $ w { gameState = WaitingBoth      }
      False -> return $ w { gameState = RequestButtonsUp }
  | isConfirmReset gameState && k == Char 'y' =
    return $ w {gameState = WaitingBoth }
  | isConfirmReset gameState && k == Char 'n' =
      let (ConfirmReset oldState) = gameState in
      return $ w { gameState = oldState }
  | gameState == RequestButtonsUp = do
        allOff <- allBtnsOff w
        case allOff of
          True  -> return $ w {gameState = WaitingBoth}
          False -> return $ w
  | otherwise = return $ w {gameState = ConfirmReset gameState}


------------------------------------------------------------------------------
allBtnsOff :: World -> IO Bool
allBtnsOff w = not <$> foldM (\a (p,c) -> (|| a) <$> readButton w p c) False
               [(p,c) | p <- [PlayerLeft,PlayerRight], c <- [Cooperate, Cheat]]
        

------------------------------------------------------------------------------
readButton :: World -> Player -> Choice -> IO Bool
readButton World{..} p c = do
    let f = case (p,c) of
          (PlayerLeft,Cooperate)  -> leftCoopFile
          (PlayerLeft,Cheat)      -> leftCheatFile
          (PlayerRight,Cooperate) -> rightCoopFile
          (PlayerRight,Cheat)     -> rightCheatFile
    fc <- oneoffRead f
    case listToMaybe fc of
      Just '1' -> return True
      Just '0' -> return False
      _   -> putStrLn (f ++ ", Bad contents: " ++ fc) >> return False


------------------------------------------------------------------------------
readChoice :: World -> Player -> IO (Maybe Choice)
readChoice w@World{..} p = do
  pCoop  <- readButton w p Cooperate
  pCheat <- readButton w p Cheat
  case (pCoop,pCheat) of
    (False,False) -> return Nothing
    (_,    True)  -> return $ Just Cheat     -- Cheat or both btns -> "Cheat"
    _             -> return $ Just Cooperate -- Only Coop button   -> "Coop"


------------------------------------------------------------------------------
timestep :: Float -> World -> IO World
timestep f w = do
  w' <- timestep' f w
  threadDelay 10000
  return w'

oneoffRead :: FilePath -> IO String
oneoffRead fn = withFile fn ReadMode $ \h -> do
  hSetBuffering h NoBuffering
  s <- hGetContents h
  return $! s

oneoffWrite :: FilePath -> String -> IO ()
oneoffWrite fn s = withFile fn WriteMode $ \h -> do
  hSetBuffering h NoBuffering
  hPutStrLn h s

------------------------------------------------------------------------------
timestep' :: Float -> World -> IO World
timestep' _ w@World{..}
  | gameState == WaitingBoth = do
    someButtonDown <- not <$> allBtnsOff w
    case someButtonDown of
      False -> return $ w
      True  -> do
        leftChoice  <- readChoice w PlayerLeft
        rightChoice <- readChoice w PlayerRight
        case (leftChoice, rightChoice) of
          (Nothing, Nothing) -> return w
          (Just c,  Nothing) -> return $ w { gameState = GotOne PlayerLeft  c }
          (Nothing, Just c ) -> return $ w { gameState = GotOne PlayerRight c }
          (Just l,  Just r ) -> return $ w { gameState = GotBoth l r        }
  | isGotOne gameState = do
    let (GotOne p c) = gameState
    newChoice <- readChoice w (otherPlayer p)
    case newChoice of
      Nothing   -> return w
      (Just c') -> let (leftC, rightC) = case p of
                         PlayerLeft  -> (c,  c')
                         PlayerRight -> (c', c ) in
        return $ w { gameState = GotBoth leftC rightC }
  | isGotBoth gameState           = return w
  | isConfirmReset gameState      = return w
  | RequestButtonsUp == gameState = return w
  | otherwise                     = error "Impossible case in timestep"

------------------------------------------------------------------------------ 
data World = World {
    width          :: Float
  , height         :: Float
  , resources      :: Map.Map String Picture
  , leftCoopFile   :: FilePath
  , leftCheatFile  :: FilePath
  , rightCoopFile  :: FilePath
  , rightCheatFile :: FilePath
  , gameState      :: WorldState
  }

data WorldState = WaitingBoth
                | ConfirmReset WorldState
                | RequestButtonsUp
                | GotOne  Player Choice
                | GotBoth Choice Choice
                deriving (Eq)

isConfirmReset :: WorldState -> Bool
isConfirmReset (ConfirmReset _) = True
isConfirmReset _                = False

isGotOne :: WorldState -> Bool
isGotOne (GotOne _ _) = True
isGotOne _            = False

data Choice = Cooperate
            | Cheat
            deriving (Eq)

isGotBoth :: WorldState -> Bool
isGotBoth (GotBoth _ _ ) = True
isGotBoth _              = False


data Player = PlayerLeft
            | PlayerRight
            deriving (Eq)

otherPlayer :: Player -> Player
otherPlayer PlayerLeft  = PlayerRight
otherPlayer PlayerRight = PlayerLeft

data Result = BothCooperated
            | BothCheated
            | OneCheated Player
            deriving (Eq)

