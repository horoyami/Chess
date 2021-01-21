module GameCycle where

import System.Console.ANSI

import Checks
import Board
import Figures

gameCycle :: IO ()
gameCycle = do
  makeMove makeEmptyBoard 1

makeMove :: [[Cell]] -> Int -> IO ()
makeMove board player = do
  putStrLn ("Player" ++ (show player) ++" makes a move (a1-h8):")
  input <- getLine
  let coords = parseNote input
  if coords /= Nothing then do
      let (Just (x1, y1, x2, y2)) = coords
      let figureColor = if player == 1 then whiteFigure else blackFigure
      newBoard <- move board 1 figureColor x1 y1 x2 y2
      let isBadMove = newBoard == board
      let text = if isBadMove then "Bad move!" else ("Player" ++ (show player) ++" made a move " ++ input)
      reshowScreen newBoard text
      makeMove board (player `mod` 2 + (if isBadMove then 0 else 1))
    else do
      cursorUp 3
      clearFromCursorToScreenEnd
      putStrLn "Bad input!"
      makeMove board player

reshowScreen :: [[Cell]] -> String -> IO ()
reshowScreen board text = do
  clearScreen
  setCursorPosition 0 0
  showBoard board
  putStr "\n"
  putStrLn text