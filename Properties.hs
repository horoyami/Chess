module Properties where

import Figures

getFigure :: [[Cell]] -> Int -> Int -> Cell
getFigure board x y = head (drop (x-1) (head (drop (y-1) board)))