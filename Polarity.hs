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

canPutHorizontal :: [String] -> Int -> Int -> String -> Bool
canPutHorizontal board i j str
  | j-1 >= 0 && board !! i !! (j-1) == head str = False
  | i-1 >= 0 && board !! (i-1) !! j == head str = False
  | i-1 >= 0 && board !! (i-1) !! (j+1) == str !! 1 = False
  | j+2 < length (head board) && board !! i !! (j+2) == str !! 1 = False
  | otherwise = True

canPutVertical :: [String] -> Int -> Int -> String -> Bool
canPutVertical board i j str
  | j-1 >= 0 && board !! i !! (j-1) == head str = False
  | i-1 >= 0 && board !! (i-1) !! j == head str = False
  | j+1 < length (head board) && board !! i !! (j+1) == head str = False
  | otherwise = True

{-
checkSpecs :: [Int] -> [Int] -> [Int] -> [Int] -> Bool
checkSpecs left right top bot =
  let
    posCountHor = replicate (length board) 0
    negCountHor = replicate (length board) 0
    posCountVer = replicate (length (head board)) 0
    negCountVer = replicate (length (head board)) 0
  in
    checkSpecs2 left right top bot posCountHor negCountHor posCountVer negCountVer



hLoop :: [String] -> Int -> Int -> [Int] -> [Int] -> ([Int], [Int])
hLoop [] _ _ _ _ = []
hLoop (x:xs) i j posCountHor negCountHor
  | x == [+] = 
    let posNew = mutateList posCountHor i
    in hLoop xs (i+1) j (posNew, negCountHor)
  | x == [-] =
    let negNew = mutateList negCountHor i
    in hLoop xs (i+1) j (posCountHor, negNew) 
  | otherwise = hLoop xs (i+1) j (posCountHor, negCountHor) 

vLoop :: [String] -> Int -> Int -> [Int] -> [Int] -> ([Int], [Int])
vLoop [] _ _ _ _ = []
vLoop (x:xs) i j posCountVer negCountVer
  | x == [+] =  
    let posNew = mutateList posCountVer j
    in vLoop (xs) i (j+1) (posNew, negCountVer)
  | x == [-] =
    let negNew = mutateList negCountVer j
    in vLoop (xs) i (j+1) (posCountVer, negNew) 
  | otherwise = vLoop (xs) i (j+1) (posCountVer, negCountVer) 

mutateList :: [Int] -> Int -> [Int]
mutateList old index =
  take index old ++ [(old !! index) + 1] ++ drop (index+1) old
-}

mutateBoard :: [String] -> Int -> Int -> Char -> [String]
mutateBoard board i j newVal =
  take i board ++
  [mutateRow (board !! i) j newVal] ++
  drop (i+1) board
  where
    mutateRow :: String -> Int -> Char -> String
    mutateRow str col newVal =
      take col str ++ [newVal] ++ drop (col+1) str

checkSpecs :: [String] -> [Int] -> [Int] -> [Int] -> [Int] -> Bool
checkSpecs board left right top bot = True


-- if i == length board && j == 0 then
solvePuzzle :: [String] -> [Int] -> [Int] -> [Int] -> [Int] -> Int -> Int -> [String]
solvePuzzle board left right top bot i j
  | i == length board && j == 0 && checkSpecs board left right top bot = board
  | j >= length (head board) = solvePuzzle board left right top bot (i+1) 0
  | otherwise =

    -- Check for Horizontal Placements
    if board !! i !! j == 'L' then
      -- +- Condition
      if canPutHorizontal board i j "+-" then
        let
          newboard = mutateBoard board i j '+'
          newboard2 = mutateBoard newboard i (j+1) '-'
          in
            solvePuzzle newboard2 left right top bot i (j+2)
      else
        -- -+ Condition 
        if canPutHorizontal board i j "-+" then
          let
            newboard = mutateBoard board i j '-'
            newboard2 = mutateBoard board i (j+1) '+'
            in
              solvePuzzle newboard2 left right top bot i (j+2)
        else
          -- XX Condition j=x, j+1=x, j=l,j+1=R, 
          if canPutHorizontal board i j "XX" then
            let
              newboard = mutateBoard board i j 'X'
              newboard2 = mutateBoard board i (j+1) 'X'
              in
                solvePuzzle newboard2 left right top bot i (j+2)
          else
            solveHelper board left right top bot i j -- To deal with Vertical Orientations

    -- If none work, backtrack
    else
      solvePuzzle board left right top bot i (j+1)


solveHelper :: [String] -> [Int] -> [Int] -> [Int] -> [Int] -> Int -> Int -> [String]
solveHelper board left right top bot i j
  | board !! i !! j == 'T' =
    -- +- Condition
      if canPutVertical board i j "+-" then
        let
          newboard = mutateBoard board i j '+'
          newboard2 = mutateBoard board (i+1) j '-'
        in
          solvePuzzle newboard2 left right top bot i (j+1)
      else
        -- -+ Condition 
        if canPutVertical board i j "-+" then
          let
            newboard = mutateBoard board i j '-'
            newboard2 = mutateBoard board (i+1) j '+'
          in
            solvePuzzle newboard2 left right top bot i (j+1)
        else
          -- XX Condition
          if canPutVertical board i j "XX" then
            let
              newboard = mutateBoard board i j 'X'
              newboard2 = mutateBoard board (i+1) j 'X'
            in
              solvePuzzle newboard2 left right top bot i (j+1)
          else
            solvePuzzle board left right top bot i (j+1)

  -- [ "+-+-X-" , "-+-+X+", "XX+-+-", "XX-+X+", "-+XXX-" ]
