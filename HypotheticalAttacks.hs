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
      hypotheticalAttacks = if f /= Empty && c f == color then getHypotheticalAttacks board x y else []

getHypotheticalAttacks :: Board -> Int -> Int -> [(Int, Int)]
getHypotheticalAttacks board x y =
  if f == Empty then []
  else if f == wp then getWhitePawnHypotheticalAttacks x y
  else if f == bp then getBlackPawnHypotheticalAttacks x y
  else if f == bk || f == wk then getKingHypotheticalAttacks x y
  else if f == wr || f == br then getRookHypotheticalAttacks board x y
  else []
  where
    f = getFigure board x y

getHypotheticalLineAttacks :: Board -> ((Int, Int) -> (Int, Int)) -> (Int, Int) -> [(Int, Int)]
getHypotheticalLineAttacks board lambda (x, y) | y == 9 || x == 9 || y == 0 || x == 0 = []
                                               | getFigure board x y == Empty = [(x, y)] ++ getHypotheticalLineAttacks board lambda (lambda (x, y))
                                               | otherwise = [(x, y)]

getHypotheticalStraightAttacks :: Board -> Int -> Int -> [(Int, Int)]
getHypotheticalStraightAttacks board x y = filter cellFilter (
 (getHypotheticalLineAttacks board (\(x, y) -> (x, y+1)) (x, y+1)) ++ (getHypotheticalLineAttacks board (\(x, y) -> (x, y-1)) (x, y-1)) ++
 (getHypotheticalLineAttacks board (\(x, y) -> (x-1, y)) (x-1, y)) ++ (getHypotheticalLineAttacks board (\(x, y) -> (x+1, y)) (x+1, y)))

getHypotheticalDiagonalAttacks :: Board -> Int -> Int -> [(Int, Int)]
getHypotheticalDiagonalAttacks board x y = filter cellFilter (
 (getHypotheticalLineAttacks board (\(x, y) -> (x+1, y+1)) (x+1, y+1)) ++ (getHypotheticalLineAttacks board (\(x, y) -> (x-1, y-1)) (x-1, y-1)) ++
 (getHypotheticalLineAttacks board (\(x, y) -> (x-1, y+1)) (x-1, y+1)) ++ (getHypotheticalLineAttacks board (\(x, y) -> (x+1, y-1)) (x+1, y-1)))

getWhitePawnHypotheticalAttacks :: Int -> Int -> [(Int, Int)]
getWhitePawnHypotheticalAttacks x y = filter cellFilter [(x+1, y+1), (x-1, y+1)]

getBlackPawnHypotheticalAttacks :: Int -> Int -> [(Int, Int)]
getBlackPawnHypotheticalAttacks x y = filter cellFilter [(x+1, y-1), (x-1, y-1)]

getKingHypotheticalAttacks :: Int -> Int -> [(Int, Int)]
getKingHypotheticalAttacks x y = filter cellFilter [(x, y+1), (x+1, y+1), (x+1, y), (x+1, y-1), (x, y-1), (x-1, y-1), (x-1, y), (x-1, y+1)]

getRookHypotheticalAttacks :: Board -> Int -> Int -> [(Int, Int)]
getRookHypotheticalAttacks board x y = getHypotheticalStraightAttacks board x y

getBishopHypotheticalAttacks :: Board -> Int -> Int -> [(Int, Int)]
getBishopHypotheticalAttacks board x y = getHypotheticalDiagonalAttacks board x y