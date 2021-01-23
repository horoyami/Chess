module Board where

import System.Console.ANSI

import Figures

makeEmptyBoard = [
  [wr, wh, wb, wq, wk, wb, wh, wr],
  [wp, wp, wp, wp, wp, wp, wp, wp],
  [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
  [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
  [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
  [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
  [bp, bp, bp, bp, bp, bp, bp, bp],
  [bp, bh, bb, bq, bk, bb, bh, bp]
  ]

data Status = Checkmate | Check | Stalemate | Proceed deriving (Show, Eq)

showCell :: Cell -> Int -> Int -> IO ()
showCell cell n m = do
  let bcolor = if (n+m) `mod` 2 == 0 then blackCell else whiteCell
  setSGR [SetPaletteColor Background bcolor]

  if cell == Empty then do
    putStr "   "
  else do
    let (WithFigure figure color _) = cell
    setSGR [SetPaletteColor Foreground color]
    putStr [' ', figure, ' ']

showRow :: Row -> Int -> IO ()
showRow row n = do
  let (x1 : x2 : x3 : x4: x5 : x6 : x7 : x8 : _) = row
  showCell x1 n 1
  showCell x2 n 2
  showCell x3 n 3
  showCell x4 n 4
  showCell x5 n 5
  showCell x6 n 6
  showCell x7 n 7
  showCell x8 n 8
  setSGR [SetDefaultColor Background]
  setSGR [SetDefaultColor Foreground]
  putStr (" " ++ (show n) ++ "\n")

showBoard :: Board -> IO ()
showBoard board = do
  let (y1 : y2 : y3 : y4: y5 : y6 : y7 : y8 : _) = board
  showRow y8 8
  showRow y7 7
  showRow y6 6
  showRow y5 5
  showRow y4 4
  showRow y3 3
  showRow y2 2
  showRow y1 1
  setSGR [SetDefaultColor Background]
  setSGR [SetDefaultColor Foreground]
  putStr " A  B  C  D  E  F  G  H \n"

changeBoard :: Board -> Int -> Int -> Int -> Int -> Board
changeBoard board x1 y1 x2 y2 = newBoard where
  figure = (getFigure board x1 y1) {m = True}
  board1 = setFigure board x2 y2 figure
  newBoard = setFigure board1 x1 y1 Empty

setFigure :: Board -> Int -> Int -> Cell -> Board
setFigure board x y cell = firstPart ++ [changeRow row x cell] ++ secondPart where
  firstPart = take (y-1) board
  row = head (drop (y-1) board)
  secondPart = drop (y) board

changeRow :: Row -> Int -> Cell -> Row
changeRow row x cell = (take (x-1) row) ++ [cell] ++ (drop x row)