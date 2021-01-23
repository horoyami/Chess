module Figures where

import Data.Word

type Color = Word8
data Cell = WithFigure {s :: Char, c :: Word8, m :: Bool} | Empty
type Row = [Cell]
type Board = [Row]

instance Eq Cell where
  (==) Empty Empty = True
  (==) _ Empty = False
  (==) Empty _ = False
  (==) (WithFigure s1 c1 _) (WithFigure s2 c2 _) = s1 == s2 && c1 == c2

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

wp = pawn whiteFigure False
bp = pawn blackFigure False
wh = knight whiteFigure False
bh = knight blackFigure False
wb = bishop whiteFigure False
bb = bishop blackFigure False
wq = queen whiteFigure False
bq = queen blackFigure False
wr = rook whiteFigure False
br = rook blackFigure False
wk = king whiteFigure False
bk = king blackFigure False

getOppositeColor :: Color -> Color
getOppositeColor color = if color == blackFigure then whiteFigure else blackFigure

getFigure :: Board -> Int -> Int -> Cell
getFigure board x y = head (drop (x-1) (head (drop (y-1) board)))

isBlack :: Cell -> Bool
isBlack Empty = False
isBlack (WithFigure _ 0 _) = True
isBlack _ = False

isWhite :: Cell -> Bool
isWhite Empty = False
isWhite cell = not (isBlack cell)

isNotSame :: Cell -> Color -> Bool
isNotSame Empty _ = True
isNotSame cell color = c cell /= color

cellFilter :: (Int, Int) -> Bool
cellFilter (x, y) = x >= 1 && x <= 8 && y >= 1 && y <= 8