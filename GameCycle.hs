module GameCycle where

import System.Console.ANSI

import Checks
import Board
import Figures

gameCycle :: IO ()
gameCycle = do
  makeMove makeEmptyBoard 1

makeMove :: [[Cell]] -> Int -> IO ()
makeMove broad player = do
  putStrLn ("Player" ++ (show player) ++" makes a move (a1-h8):")
  input <- getLine
  let coords = parseNote input
  if coords /= Nothing then do
      let (Just (x1, y1, x2, y2)) = coords
      let figureColor = if player == 1 then whiteFigure else blackFigure
      newBoard <- move broad 1 figureColor x1 y2 x2 y2
      makeMove broad player
    else do
      cursorUp 3
      clearFromCursorToScreenEnd
      putStrLn "Bad input"
      makeMove broad player