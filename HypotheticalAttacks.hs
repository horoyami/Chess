module HypotheticalAttacks where

import Data.List

import Figures

getAllHypotheticalAttacks :: Board -> Color -> [(Int, Int)]
getAllHypotheticalAttacks board color = nub (get board color 1 1) where
  get board color x y | x == 8 && y == 8 = hypotheticalAttacks
                      | x == 8 = hypotheticalAttacks ++ get board color 1 (y+1)
                      | otherwise = hypotheticalAttacks ++ get board color (x+1) y
    where
      f = getFigure board x y
      hypotheticalAttacks = getHypotheticalAttacks board color x y

getHypotheticalAttacks :: Board -> Color -> Int -> Int -> [(Int, Int)]
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