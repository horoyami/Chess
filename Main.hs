module Main where

import System.Console.ANSI

import Board
import GameCycle

main :: IO ()
main = do
  clearScreen
  setCursorPosition 0 0
  showBoard makeEmptyBoard
  putStr "\n"
  putStrLn "Welcome to Chess!!!"
  gameCycle

