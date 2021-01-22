module Figures where

import Data.Word

type Color = Word8
data Cell = WithFigure {s :: Char, c :: Word8} | Empty deriving Eq
type Row = [Cell]
type Board = [Row]

blackCell = 159 :: Color
whiteCell = 255 :: Color
blackFigure = 0 :: Color
whiteFigure = 9 :: Color

pawn = WithFigure '♟'
knight = WithFigure '♞'
bishop = WithFigure '♝'
queen = WithFigure '♛'
rook = WithFigure '♜'
king = WithFigure '♚'

wp = pawn whiteFigure
bp = pawn blackFigure
wh = knight whiteFigure
bh = knight blackFigure
wb = bishop whiteFigure
bb = bishop blackFigure
wq = queen whiteFigure
bq = queen blackFigure
wr = rook whiteFigure
br = rook blackFigure
wk = king whiteFigure
bk = king blackFigure

getOppositeColor :: Color -> Color
getOppositeColor color = if color == blackFigure then whiteFigure else blackFigure

getFigure :: Board -> Int -> Int -> Cell
getFigure board x y = head (drop (x-1) (head (drop (y-1) board)))

isBlack :: Cell -> Bool
isBlack Empty = False
isBlack (WithFigure _ 0) = True
isBlack _ = False

isWhite :: Cell -> Bool
isWhite Empty = False
isWhite cell = not (isBlack cell)

isNotSame :: Cell -> Color -> Bool
isNotSame Empty _ = True
isNotSame cell color = c cell /= color

cellFilter :: (Int, Int) -> Bool
cellFilter (x, y) = x >= 1 && x <= 8 && y >= 1 && y <= 8