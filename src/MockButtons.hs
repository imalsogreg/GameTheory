{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent
import System.IO
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data World = World

main = do
  playIO (InWindow "Mock buttons" (100,100) (0,500))
    white 5 (World) draw input timestep

draw :: World -> IO Picture
draw w = do
  a <- oneoffRead leftCoopFile
  b <- oneoffRead leftCheatFile
  c <- oneoffRead rightCoopFile
  d <- oneoffRead rightCheatFile
  putStrLn $ (map head [a,b,c,d])
  return $ circle 10

input :: Event -> World -> IO World
input (EventKey (Char k) d _ _) w
  | k `elem` "aoAO" = overwriteFile w k d >> return w
input _ w = return w

leftCoopFile = "a.txt"
leftCheatFile = "b.txt"
rightCoopFile = "c.txt"
rightCheatFile = "d.txt"

oneoffRead :: FilePath -> IO String
oneoffRead fn = withFile fn ReadMode $ \h -> do
  hSetBuffering h NoBuffering
  s <- hGetContents h
  return $! s

oneoffWrite :: FilePath -> String -> IO ()
oneoffWrite fn s = do
  withFile fn WriteMode $ \h -> do
    hSetBuffering h NoBuffering
    hPutStrLn h s
  threadDelay 50000

overwriteFile :: World -> Char -> KeyState -> IO ()
overwriteFile World{..} 'a' Down = oneoffWrite leftCoopFile   "1"
overwriteFile World{..} 'a' Up   = oneoffWrite leftCoopFile   "0"
overwriteFile World{..} 'o' Down = oneoffWrite leftCheatFile  "1"
overwriteFile World{..} 'o' Up   = oneoffWrite leftCheatFile  "0"
overwriteFile World{..} 'A' Down = oneoffWrite rightCoopFile  "1"
overwriteFile World{..} 'A' Up   = oneoffWrite rightCoopFile  "0"
overwriteFile World{..} 'O' Down = oneoffWrite rightCheatFile "1"
overwriteFile World{..} 'O' Up   = oneoffWrite rightCheatFile "0"
overwriteFile _ _ _              = error "Impossible case in overwriteFile"


timestep :: Float -> World -> IO World
timestep _ w = return w
