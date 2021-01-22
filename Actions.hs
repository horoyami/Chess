module Actions where

import Data.Word

import Figures
import Board
import Properties
import Checks

move :: [[Cell]] -> Int -> Word8 -> Int -> Int -> Int -> Int -> IO [[Cell]]
move board step playerColor x1 y1 x2 y2 | (x2, y2) `elem` getAvailableMoves board playerColor x1 y1 = return (changeBoard board x1 y1 x2 y2)
                                        | otherwise = return board