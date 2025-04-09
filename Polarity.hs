module Polarity where

import Data.Array
import Control.Monad
import Data.List
import Data.Maybe

type Grid = Array (Int, Int) Char

polarity :: [String] -> ([Int], [Int], [Int], [Int]) -> [String]
polarity board specs =
  let (left, right, top, bot) = specs
  in solvePuzzle board left right top bot 0 0

{-
validBoard :: [String] -> Bool
validBoard board = 
  let rows = length board
      cols = if null board then 0 else length (head board)
      grid = listArray ((0,0), (rows-1,cols-1)) (concat board)
    
      -- in all (`elem` "LRBTX+-") (concat board) && validPairs grid
-}

checkSpecs :: [Int] -> [Int] -> [Int] -> [Int] -> Bool
checkSpecs left right top bot = True
  
mutateBoard :: [String] -> Int -> Int -> Char -> [String]
mutateBoard board i j newVal =
  take i j board ++
  [mutateRow (board !! i) j newVal] ++
  drop (i+1) board
  where
    mutateRow :: String -> Int -> Char -> String
    mutateRow str col newVal =
      take col str ++ [newVal] ++ drop (col+1) str



-- if i == length board && j == 0 then
solvePuzzle :: [String] -> [Int] -> [Int] -> [Int] -> [Int] -> Int -> Int -> [String]
solvePuzzle board left right top bot i j
  | i == length board && j == 0 && checkSpecs left right top bot = print(rules)
  | j >= length rules !! 0 = solvePuzzle board left right top bot (i+1) 0
  | otherwise =
    
    -- Check for Horizontal Placements
    if board !! i !! j == "L" then
      -- +- Condition
      if canPutHorizontal board i j "+-" then
        let
          newboard = mutateBoard board i j "+"
          newboard2 = mutateBoard newboard i j+1 "-"
          in 
            solvePuzzle newboard2 left right top bot i j+2
          
      -- -+ Condition 
      if canPutHorizontal board i j "-+" then
        let
          newboard = mutateBoard board i j "-"
          newboard2 = mutateBoard board i j+1 "+"
          in 
            solvePuzzle newboard2 left right top bot i j+2

      -- XX Condition j=x, j+1=x, j=l,j+1=R, 
      if canPutHorizontal board i j "XX" then
        let
          newboard = mutateBoard board i j "X"
          newboard2 = mutateBoard board i j+1 "X"
          in 
            solvePuzzle newboard2 left right top bot i j+2

    -- Check for Vertical Placements
    if board !! i !! j == "T" then
      -- +- Condition
      if canPutVertical board i j "+-" then
        let
          newboard = mutateBoard board i j "+"
          newboard2 = mutateBoard board i+1 j "-"
        in
          solvePuzzle newboard2 left right top bot i j+1
      
      -- -+ Condition 
      if canPutVertical board i j "-+" then
        let
          newboard = mutateBoard board i j "-"
          newboard2 = mutateBoard board i+1 j "+"
        in
          solvePuzzle newboard2 left right top bot i j+1

      -- XX Condition
      if canPutVertical board i j "XX" then
        let
          newboard = mutateBoard board i j "X"
          newboard2 = mutateBoard board i+1 j "X"
        in
          solvePuzzle newboard2 left right top bot i j+1
          
    -- If none work, backtrack
    solvePuzzle board left right top bot i j+1

  -- [ "+-+-X-" , "-+-+X+", "XX+-+-", "XX-+X+", "-+XXX-" ]
