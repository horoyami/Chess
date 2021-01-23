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

move :: Board -> Color -> Maybe(Int, Int, Int, Int) -> Board
move board color (Just (x1, y1, x2, y2)) | canMove && not isRiskForKing = changeBoard board x1 y1 x2 y2
                                         | otherwise = board
  where
    f = getFigure board x1 y1
    canMove = f /= Empty && c f == color && (x2, y2) `elem` getAvailableMoves board x1 y1
    newBoard = changeBoard board x1 y1 x2 y2
    isRiskForKing = getKingStatus newBoard color == Checkmate || getKingStatus newBoard color == Check
move board _ _ = board

castling :: Board -> Color -> String -> Board
castling board color com = if com == "0-0-0" then leftCastlingBoard else rightCastlingBoard
  where
    y = if color == whiteFigure then 1 else 8
    x = if com == "0-0-0" then 1 else 8
    isKing = getFigure board 5 y /= Empty && not (m (getFigure board 5 y))
    isRook = getFigure board x y /= Empty && not (m (getFigure board x y))
    enemyAttacks = getAllAvailableMoves board (getOppositeColor color)

    rightAttacked = (5, y) `elem` enemyAttacks || (6, y) `elem` enemyAttacks || (7, y) `elem` enemyAttacks
    rightPlace = getFigure board 6 y == Empty && getFigure board 7 y == Empty
    rightCastling = isKing && isRook && rightPlace && not rightAttacked
    rightCastlingBoard = if rightCastling then changeBoard (changeBoard board 8 y 6 y) 5 y 7 y else board

    leftAttacked = (5, y) `elem` enemyAttacks || (4, y) `elem` enemyAttacks || (3, y) `elem` enemyAttacks
    leftPlace = getFigure board 4 y == Empty && getFigure board 3 y == Empty && getFigure board 2 y == Empty
    leftCastling = isKing && isRook && rightPlace && not rightAttacked
    leftCastlingBoard = if rightCastling then changeBoard (changeBoard board 1 y 4 y) 5 y 3 y else board

getComment :: Status -> Bool -> Int -> String -> String
getComment _ True _ _ = "Bad move!"
getComment Checkmate _ player _ = "Checkmate!!! Player" ++ (show player) ++ " is won!!!!"
getComment Stalemate _ _ _ = "That is Stalemate"
getComment Check _ player input = "Check!! Player" ++ (show player) ++" made a move " ++ input
getComment Proceed _ player input = "Player" ++ (show player) ++" made a move " ++ input

findKing :: Board -> Color -> (Int, Int)
findKing board color = get board color 1 1 where
  get board color x y | getFigure board x y == king color False = (x, y)
                      | x == 8 && y == 8 = (-1, -1)
                      | x == 8 = get board color 1 (y+1)
                      | otherwise = get board color (x+1) y

getKingStatus :: Board -> Color -> Status
getKingStatus board color | isBeaten && not (isParried board color 1 1) = Checkmate
                          | allAvailableMoves == [] = Stalemate
                          | isBeaten = Check
                          | otherwise = Proceed
  where
    king = findKing board color
    availableMoves = getAvailableMovesForKing board color (fst king) (snd king)
    allAvailableMoves = getAllAvailableMoves board color
    isBeaten = isKingBeaten board color

isCastling :: String -> Bool
isCastling s = s == "0-0" || s == "0-0-0"

isKingBeaten :: Board -> Color -> Bool
isKingBeaten board color = (findKing board color) `elem` (getAllAvailableMoves board (getOppositeColor color))


isParried board color x y | isOur && isHero = True
                          | x == 8 && y == 8 = False
                          | x == 8 = isParried board color 1 (y+1)
                          | otherwise = isParried board color (x+1) y
  where
    f = getFigure board x y
    isOur = f /= Empty && c f == color
    availableMoves = getAvailableMoves board x y
    check (x2, y2) = not (isKingBeaten (changeBoard board x y x2 y2) color)
    isHero = not (null (filter check availableMoves))