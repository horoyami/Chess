module GameTools where

import Data.Char

import Figures
import Board
import HypotheticalAttacks
import AvailableMoves

parseNote :: String -> Maybe (Int, Int, Int, Int)
parseNote (s1 : s2 : '-' : s3 : s4 : []) | check s1 s2 s3 s4 = Just (letterToCoord s1, digitToInt s2, letterToCoord s3, digitToInt s4)
                                         | otherwise = Nothing
    where
      letterToCoord s = ord (toLower s) - ord 'a' + 1
      check a b c d = checkL(a) && checkD(b) && checkL(c) && checkD(d)
      checkL c = c >= 'a' && c <= 'h' || c >= 'A' && c <= 'H'
      checkD c = c >= '1' && c <= '8'
parseNote _ = Nothing

move :: Board -> Color -> Int -> Int -> Int -> Int -> Board
move board color x1 y1 x2 y2 | c (getFigure board x1 y1) == color && (x2, y2) `elem` getAvailableMoves board x1 y1 = changeBoard board x1 y1 x2 y2
                             | otherwise = board

getComment :: Status -> Bool -> Int -> String -> String
getComment _ True _ _ = "Bad move!"
getComment Checkmate _ player _ = "Player" ++ (show player) ++ " is won!!!!"
getComment Stalemate _ _ _ = "That is Stalemate"
getComment Check _ player input = "Check!! Player" ++ (show player) ++" made a move " ++ input
getComment Proceed _ player input = "Player" ++ (show player) ++" made a move " ++ input

findKing :: Board -> Color -> (Int, Int)
findKing board color = get board color 1 1 where
  get board color x y | getFigure board x y == king color = (x, y)
                      | x == 8 && y == 8 = (-1, -1)
                      | x == 8 = get board color 1 (y+1)
                      | otherwise = get board color (x+1) y

getKingStatus :: Board -> Color -> Status
getKingStatus board color | isBeaten && availableMoves == [] = Checkmate
                          | allAvailableMoves == [] = Stalemate
                          | isBeaten = Check
                          | otherwise = Proceed
  where
    king = findKing board color
    availableMoves = getAvailableMovesForKing board color (fst king) (snd king)
    allAvailableMoves = getAllAvailableMoves board color
    isBeaten = king `elem` (getAllAvailableMoves board (getOppositeColor color))