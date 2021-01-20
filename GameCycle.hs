module GameCycle where

import System.Console.ANSI

import Checks

gameCycle :: IO ()
gameCycle = do
  makeMove 1

makeMove :: Int -> IO ()
makeMove player = do
  putStrLn ("Player" ++ (show player) ++" makes a move (a1-h8):")
  input <- getLine
  let coords = parseNote input
  if coords /= Nothing then do
      let (Just (x1, y1, x2, y2)) = coords
--      isAvailableMove input
      putStrLn ((show x1) ++ " " ++ (show y1) ++ " " ++ (show x2) ++ " " ++ (show y2) )
    else do
      cursorUp 3
      clearFromCursorToScreenEnd
      putStrLn "Bad input"
      makeMove player