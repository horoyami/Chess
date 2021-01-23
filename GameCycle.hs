module GameCycle where

import System.Console.ANSI

import Figures
import Board
import GameTools

makeMove :: Board -> Int -> IO ()
makeMove board player = do
  putStrLn ("Player" ++ (show player) ++" makes a move (a1-h8),(0-0-0),(0-0):")
  input <- getLine
  let coords = parseNote input
  if coords /= Nothing || isCastling input then do
      let figureColor = if player == 1 then whiteFigure else blackFigure

      let newBoard = if isCastling input then castling board figureColor input else move board figureColor coords

      let status = getKingStatus newBoard (getOppositeColor figureColor)
      let isBadMove = newBoard == board
      let text = getComment status isBadMove player input
      let nextPlayer = if isBadMove then player else player `mod` 2 + 1

      reshowScreen newBoard text

      if (status == Checkmate || status == Stalemate) then return () else makeMove newBoard nextPlayer
  else do
      cursorUp 3
      clearFromCursorToScreenEnd
      putStrLn "Bad input!"
      makeMove board player

reshowScreen :: Board -> String -> IO ()
reshowScreen board text = do
  clearScreen
  setCursorPosition 0 0
  showBoard board
  putStr "\n"
  putStrLn text