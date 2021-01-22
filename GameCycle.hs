module GameCycle where

import System.Console.ANSI

import Checks
import Board
import Figures
import Actions
import Properties

gameCycle :: IO ()
gameCycle = do
  makeMove makeEmptyBoard 1 1

getComment :: Status -> Bool -> Int -> String -> String
getComment _ True _ _ = "Bad move!"
getComment Checkmate _ player _ = "Player" ++ (show player) ++ " is won!!!!"
getComment Stalemate _ _ _ = "That is Stalemate"
getComment Check _ player input = "Check!! Player" ++ (show player) ++" made a move " ++ input
getComment Proceed _ player input = "Player" ++ (show player) ++" made a move " ++ input

makeMove :: [[Cell]] -> Int -> Int -> IO ()
makeMove board player step = do
  putStrLn ("Player" ++ (show player) ++" makes a move (a1-h8):")
  input <- getLine
  let coords = parseNote input
  if coords /= Nothing then do
      let (Just (x1, y1, x2, y2)) = coords
      let figureColor = if player == 1 then whiteFigure else blackFigure
      newBoard <- move board step figureColor x1 y1 x2 y2
      let status = getKingStatus newBoard (getOppositeColor figureColor)
      let isBadMove = newBoard == board
      let text = getComment status isBadMove player input
      reshowScreen newBoard text
      if (status == Checkmate || status == Stalemate) then return () else do
        let nextPlayer = player `mod` 2 + 1
        makeMove newBoard (if isBadMove then player else nextPlayer) (if isBadMove then step else (step + 1))
  else do
      cursorUp 3
      clearFromCursorToScreenEnd
      putStrLn "Bad input!"
      makeMove board player (step + 1)

reshowScreen :: [[Cell]] -> String -> IO ()
reshowScreen board text = do
  clearScreen
  setCursorPosition 0 0
  showBoard board
  putStr "\n"
  putStrLn text