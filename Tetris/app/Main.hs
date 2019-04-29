module Main where

import Lib
import Graphics.UI.GLUT
import Data.IORef
import Data.List
import System.Random
import System.Exit
import Data.Time

main :: IO ()
main = do
  getArgsAndInitialize
  initialDisplayMode $= [RGBAMode]
  initialWindowSize $= Size 480 600
  createWindow "Tetris"
  clearColor $= Color4 0 0 0 1
  (m,g) <- randomMino . mkStdGen . (read :: String -> Int) . take 6 . drop 20 . show <$> getCurrentTime
  n <- (read :: String -> Int) . take 6 . drop 20 . show <$> getCurrentTime
  state <- newIORef $ State (m, (div width 2,height+2), Ue) [] [] (random5Minos n,g) (level 0) 0 False
  displayCallback $= display state
  keyboardCallback $= Just (keyboard state)
  addTimerCallback 64 (timerProc (timer state))
  reshapeCallback $= Just resize
  idleCallback $= Just idle
  mainLoop
