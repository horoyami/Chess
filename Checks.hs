module Checks where

import Data.Char
import Data.Word

import Figures
import Board

parseNote :: String -> Maybe (Int, Int, Int, Int)
parseNote (s1 : s2 : '-' : s3 : s4 : []) | check s1 s2 s3 s4 = Just (letterToCoord s1, digitToInt s2, letterToCoord s3, digitToInt s4)
                                         | otherwise = Nothing
    where
      letterToCoord s = ord (toLower s) - ord 'a' + 1
      check a b c d = checkL(a) && checkD(b) && checkL(c) && checkD(d)
      checkL c = c >= 'a' && c <= 'h' || c >= 'A' && c <= 'H'
      checkD c = c >= '1' && c <= '8'
parseNote _ = Nothing

move :: [[Cell]] -> Int -> Word8 -> Int -> Int -> Int -> Int -> IO [[Cell]]
move board step playerColor x1 y1 x2 y2 | getFigure board x1 y1 == pawn playerColor = return (movePawn board playerColor step x1 y1 x2 y2)
                                        | otherwise = return board

movePawn :: [[Cell]] -> Word8 -> Int -> Int -> Int -> Int -> Int -> [[Cell]]
movePawn board playerColor step x1 y1 x2 y2 = do
  let x = x2 - x1
  let y = y2 - y1

  if playerColor == whiteFigure && (x, y) == (0, 1) then moveWhitePawnOneStep board x1 y1
  else if playerColor == blackFigure && (x, y) == (0, -1) then moveBlackPawnOneStep board x1 y1
  else if playerColor == whiteFigure && (x, y) == (0, 2) && y1 == 2 then moveWhitePawnTwoStep board x1 y1
  else if playerColor == blackFigure && (x, y) == (0, -2) && y1 == 7 then moveBlackPawnTwoStep board x1 y1
  else if playerColor == whiteFigure && ((x, y) == (1, 1) || (x, y) == (-1, 1)) then attackWhitePawn board x1 y1 x2 y2
  else if playerColor == blackFigure && ((x, y) == (-1, -1) || (x, y) == (1, -1)) then attackBlackPawn board x1 y1 x2 y2
  else board

attackWhitePawn :: [[Cell]] -> Int -> Int -> Int -> Int -> [[Cell]]
attackWhitePawn board x1 y1 x2 y2 = if isBlack (getFigure board x2 y2) then changeBoard board x1 y1 x2 y2 else board

attackBlackPawn :: [[Cell]] -> Int -> Int -> Int -> Int -> [[Cell]]
attackBlackPawn board x1 y1 x2 y2 = if isWhite (getFigure board x2 y2) then changeBoard board x1 y1 x2 y2 else board

moveWhitePawnTwoStep :: [[Cell]] -> Int -> Int -> [[Cell]]
moveWhitePawnTwoStep board x1 y1 = if getFigure board x1 (y1+1) /= Empty || getFigure board x1 (y1+2) /= Empty
                                     then board else changeBoard board x1 y1 x1 (y1+2)

moveBlackPawnTwoStep :: [[Cell]] -> Int -> Int -> [[Cell]]
moveBlackPawnTwoStep board x1 y1 = if getFigure board x1 (y1-1) /= Empty || getFigure board x1 (y1-2) /= Empty
                                     then board else changeBoard board x1 y1 x1 (y1-2)

moveWhitePawnOneStep :: [[Cell]] -> Int -> Int -> [[Cell]]
moveWhitePawnOneStep board x1 y1 = if getFigure board x1 (y1+1) /= Empty then board else changeBoard board x1 y1 x1 (y1+1)

moveBlackPawnOneStep :: [[Cell]] -> Int -> Int -> [[Cell]]
moveBlackPawnOneStep board x1 y1 = if getFigure board x1 (y1-1) /= Empty then board else changeBoard board x1 y1 x1 (y1-1)