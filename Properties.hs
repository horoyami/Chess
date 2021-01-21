module Properties where

import Data.Word
import Data.List

import Figures
import Checks

getFigure :: [[Cell]] -> Int -> Int -> Cell
getFigure board x y = head (drop (x-1) (head (drop (y-1) board)))

getAvailableMoves :: [[Cell]] -> Int -> Int -> [(Int, Int)]
getAvailableMoves board x y =
  if f == wp then getAvailableMovesForWhitePawn board x y
  else if f == bp then getAvailableMovesForBlackPawn board x y
  else []
  where
    f = getFigure board x y

getAvailableMovesForWhitePawn :: [[Cell]] -> Int -> Int -> [(Int, Int)]
getAvailableMovesForWhitePawn board x y = filter cellFilter [
    if getFigure board x (y+1) == Empty then (x, y+1) else (-1,-1),
    if isBlack (getFigure board (x+1) (y+1)) then (x+1, y+1) else (-1, -1),
    if isBlack (getFigure board (x-1) (y+1)) then (x-1, y+1) else (-1, -1),
    if getFigure board x (y+1) == Empty && getFigure board x (y+2) == Empty then (x, y+2) else (-1,-1)
  ]

getAvailableMovesForBlackPawn :: [[Cell]] -> Int -> Int -> [(Int, Int)]
getAvailableMovesForBlackPawn board x y = filter cellFilter [
    if getFigure board x (y-1) == Empty then (x, y-1) else (-1,-1),
    if isWhite (getFigure board (x+1) (y-1)) then (x+1, y-1) else (-1, -1),
    if isWhite (getFigure board (x-1) (y-1)) then (x-1, y-1) else (-1, -1),
    if getFigure board x (y-1) == Empty && getFigure board x (y-2) == Empty then (x, y-2) else (-1,-1)
  ]

getAllAvailableMoves :: [[Cell]] -> Word8 -> [(Int, Int)]
getAllAvailableMoves board color = nub (get board color 1 1) where
  get board color x y | x == 8 && y == 8 = availableMoves
                      | y == 8 = availableMoves ++ get board color 1 (y+1)
                      | otherwise = availableMoves ++ get board color (x+1) y
    where
      f = getFigure board x y
      availableMoves = if f /= Empty && c f == color then getAvailableMoves board x y else []