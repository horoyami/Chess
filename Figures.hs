module Figures where

import Data.Word

data Cell = WithFigure Char Word8 | Empty deriving Eq

blackCell = 159 :: Word8
whiteCell = 255 :: Word8
blackFigure = 0 :: Word8
whiteFigure = 9 :: Word8

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

isBlack :: Cell -> Bool
isBlack Empty = False
isBlack (WithFigure _ 0) = True
isBlack _ = False

isWhite :: Cell -> Bool
isWhite Empty = False
isWhite cell = not (isBlack cell)