module Checks where

import Data.Char

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

-- TODO
isAvailableMove :: [[Cell]] -> Int -> Int -> Bool
isAvailableMove board x y = getFigure board x y