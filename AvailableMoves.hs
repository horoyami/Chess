module AvailableMoves where

import Data.List

import Figures
import Board
import HypotheticalAttacks

getAllAvailableMoves :: Board -> Color -> [(Int, Int)]
getAllAvailableMoves board color = nub (get board color 1 1) where
  get board color x y | x == 8 && y == 8 = availableMoves
                      | x == 8 = availableMoves ++ get board color 1 (y+1)
                      | otherwise = availableMoves ++ get board color (x+1) y
    where
      f = getFigure board x y
      availableMoves = if f /= Empty && c f == color then getAvailableMoves board x y else []

getAvailableMoves :: Board  -> Int -> Int -> [(Int, Int)]
getAvailableMoves board x y =
  if f == Empty then []
  else if f == wp then getAvailableMovesForWhitePawn board x y
  else if f == bp then getAvailableMovesForBlackPawn board x y
  else if f == bk then getAvailableMovesForKing board blackFigure x y
  else if f == wk then getAvailableMovesForKing board whiteFigure x y
  else if f == wr then getAvailableMovesForRook board whiteFigure x y
  else if f == br then getAvailableMovesForRook board blackFigure x y
  else if f == wb then getAvailableMovesForBishop board whiteFigure x y
  else if f == bb then getAvailableMovesForBishop board blackFigure x y
  else []
  where
    f = getFigure board x y

getAvailableMovesForWhitePawn :: Board -> Int -> Int -> [(Int, Int)]
getAvailableMovesForWhitePawn board x y = filter cellFilter [
    if cellFilter (x, y+1) && getFigure board x (y+1) == Empty then (x, y+1) else (-1,-1),
    if cellFilter (x+1, y+1) && isBlack (getFigure board (x+1) (y+1)) then (x+1, y+1) else (-1, -1),
    if cellFilter (x-1, y+1) && isBlack (getFigure board (x-1) (y+1)) then (x-1, y+1) else (-1, -1),
    if cellFilter (x, y+2) && getFigure board x (y+1) == Empty && getFigure board x (y+2) == Empty && y == 2 then (x, y+2) else (-1,-1)
  ]

getAvailableMovesForBlackPawn :: Board -> Int -> Int -> [(Int, Int)]
getAvailableMovesForBlackPawn board x y = filter cellFilter [
    if cellFilter (x, y-1) && getFigure board x (y-1) == Empty then (x, y-1) else (-1,-1),
    if cellFilter (x+1, y-1) && isWhite (getFigure board (x+1) (y-1)) then (x+1, y-1) else (-1, -1),
    if cellFilter (x-1, y-1) && isWhite (getFigure board (x-1) (y-1)) then (x-1, y-1) else (-1, -1),
    if cellFilter (x, y-2) && getFigure board x (y-1) == Empty && getFigure board x (y-2) == Empty && y == 7 then (x, y-2) else (-1,-1)
  ]

getAvailableMovesForKing :: Board -> Color -> Int -> Int -> [(Int, Int)]
getAvailableMovesForKing board color x y =
  filter (\(x2, y2) -> isAvailableKingMove board color x y x2 y2) (getKingHypotheticalAttacks x y)

isAvailableKingMove :: Board -> Color -> Int -> Int -> Int -> Int -> Bool
isAvailableKingMove board color x1 y1 x2 y2 = isNotSame (getFigure board x2 y2) color &&
  not ((x2, y2) `elem` (getAllHypotheticalAttacks (changeBoard board x1 y1 x2 y2) (getOppositeColor color)))

getAvailableMovesForRook :: Board -> Color -> Int -> Int -> [(Int, Int)]
getAvailableMovesForRook board color x y = filter (\(x,y) -> isNotSame (getFigure board x y) color) (getRookHypotheticalAttacks board x y)

getAvailableMovesForBishop :: Board -> Color -> Int -> Int -> [(Int, Int)]
getAvailableMovesForBishop board color x y = filter (\(x,y) -> isNotSame (getFigure board x y) color) (getBishopHypotheticalAttacks board x y)