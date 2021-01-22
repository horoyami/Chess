module Properties where

import Data.Word
import Data.List

import Figures
import Checks
import Board

getAvailableMoves :: [[Cell]] -> Word8 -> Int -> Int -> [(Int, Int)]
getAvailableMoves board color x y =
  if f == Empty then []
  else if f == wp && color == whiteFigure then getAvailableMovesForWhitePawn board x y
  else if f == bp && color == blackFigure  then getAvailableMovesForBlackPawn board x y
  else if f == bk && color == blackFigure  then getAvailableMovesForKing board x y whiteFigure
  else if f == wk && color == whiteFigure  then getAvailableMovesForKing board x y blackFigure
  else []
  where
    f = getFigure board x y

getAvailableMovesForWhitePawn :: [[Cell]] -> Int -> Int -> [(Int, Int)]
getAvailableMovesForWhitePawn board x y = filter cellFilter [
    if cellFilter (x, y+1) && getFigure board x (y+1) == Empty then (x, y+1) else (-1,-1),
    if cellFilter (x+1, y+1) && isBlack (getFigure board (x+1) (y+1)) then (x+1, y+1) else (-1, -1),
    if cellFilter (x-1, y+1) && isBlack (getFigure board (x-1) (y+1)) then (x-1, y+1) else (-1, -1),
    if cellFilter (x, y+2) && getFigure board x (y+1) == Empty && getFigure board x (y+2) == Empty && y == 2 then (x, y+2) else (-1,-1)
  ]

getAvailableMovesForBlackPawn :: [[Cell]] -> Int -> Int -> [(Int, Int)]
getAvailableMovesForBlackPawn board x y = filter cellFilter [
    if cellFilter (x, y-1) && getFigure board x (y-1) == Empty then (x, y-1) else (-1,-1),
    if cellFilter (x+1, y-1) && isWhite (getFigure board (x+1) (y-1)) then (x+1, y-1) else (-1, -1),
    if cellFilter (x-1, y-1) && isWhite (getFigure board (x-1) (y-1)) then (x-1, y-1) else (-1, -1),
    if cellFilter (x, y-2) && getFigure board x (y-1) == Empty && getFigure board x (y-2) == Empty && y == 7 then (x, y-2) else (-1,-1)
  ]

getAvailableMovesForKing :: [[Cell]] -> Int -> Int -> Word8 -> [(Int, Int)]
getAvailableMovesForKing board x y anticolor =
  filter (\(x2, y2) -> isAvailableKingMove board anticolor x y x2 y2) (getKingHypotheticalAttacks x y)

getAllAvailableMoves :: [[Cell]] -> Word8 -> [(Int, Int)]
getAllAvailableMoves board color = nub (get board color 1 1) where
  get board color x y | x == 8 && y == 8 = availableMoves
                      | x == 8 = availableMoves ++ get board color 1 (y+1)
                      | otherwise = availableMoves ++ get board color (x+1) y
    where
      f = getFigure board x y
      availableMoves = getAvailableMoves board color x y

isAvailableKingMove :: [[Cell]] -> Word8 -> Int -> Int -> Int -> Int -> Bool
isAvailableKingMove board anticolor x1 y1 x2 y2 =
  isNotSame (getFigure board x2 y2) anticolor && not ((x2, y2) `elem` (getAllHypotheticalAttacks (changeBoard board x1 y1 x2 y2) anticolor))

getHypotheticalAttacks :: [[Cell]] -> Word8 -> Int -> Int -> [(Int, Int)]
getHypotheticalAttacks board color x y =
  if f == Empty then []
  else if f == wp && color == whiteFigure then getWhitePawnHypotheticalAttacks x y
  else if f == bp && color == blackFigure  then getBlackPawnHypotheticalAttacks x y
  else if f == bk && color == blackFigure  then getKingHypotheticalAttacks x y
  else if f == wk && color == whiteFigure  then getKingHypotheticalAttacks x y
  else []
  where
    f = getFigure board x y

getWhitePawnHypotheticalAttacks :: Int -> Int -> [(Int, Int)]
getWhitePawnHypotheticalAttacks x y = filter cellFilter [(x+1, y+1), (x-1, y+1)]

getBlackPawnHypotheticalAttacks :: Int -> Int -> [(Int, Int)]
getBlackPawnHypotheticalAttacks x y = filter cellFilter [(x+1, y+1), (x-1, y+1)]

getKingHypotheticalAttacks :: Int -> Int -> [(Int, Int)]
getKingHypotheticalAttacks x y = filter cellFilter [(x, y+1), (x+1, y+1), (x+1, y), (x+1, y-1), (x, y-1), (x-1, y-1), (x-1, y), (x-1, y+1)]

getAllHypotheticalAttacks :: [[Cell]] -> Word8 -> [(Int, Int)]
getAllHypotheticalAttacks board color = nub (get board color 1 1) where
  get board color x y | x == 8 && y == 8 = hypotheticalAttacks
                      | x == 8 = hypotheticalAttacks ++ get board color 1 (y+1)
                      | otherwise = hypotheticalAttacks ++ get board color (x+1) y
    where
      f = getFigure board x y
      hypotheticalAttacks = getHypotheticalAttacks board color x y

findKing :: [[Cell]] -> Word8 -> (Int, Int)
findKing board color = get board color 1 1 where
  get board color x y | getFigure board x y == king color = (x, y)
                      | x == 8 && y == 8 = (-1, -1)
                      | x == 8 = get board color 1 (y+1)
                      | otherwise = get board color (x+1) y

getKingStatus :: [[Cell]] -> Word8 -> Status
getKingStatus board color | isBeaten && availableMoves == [] = Checkmate
                          | allAvailableMoves == [] = Stalemate
                          | isBeaten = Check
                          | otherwise = Proceed
  where
    king = findKing board color
    availableMoves = getAvailableMovesForKing board (fst king) (snd king) (getOppositeColor color)
    allAvailableMoves = getAllAvailableMoves board color
    isBeaten = king `elem` (getAllAvailableMoves board (getOppositeColor color))