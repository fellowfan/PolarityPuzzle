module Polarity where

import Data.Array
import Control.Monad
import Data.List
import Data.Maybe

type Grid = Array (Int, Int) Char

polarity :: [String] -> ([Int], [Int], [Int], [Int]) -> [String]
polarity board specs =
  case solvePuzzle board left right top bot 0 0 of
    Just solution -> solution
    Nothing -> []  -- Return empty list if no solution found
  where
    (left, right, top, bot) = specs

canPutHorizontal :: [String] -> Int -> Int -> String -> Bool
canPutHorizontal board i j str
  | j-1 >= 0 && (board !! i) !! (j-1) == head str = False
  | i-1 >= 0 && (board !! (i-1)) !! j == head str = False
  | i-1 >= 0 && (board !! (i-1)) !! (j+1) == str !! 1 = False
  | j+2 < length (head board) && (board !! i) !! (j+2) == str !! 1 = False
  | otherwise = True

canPutVertical :: [String] -> Int -> Int -> String -> Bool
canPutVertical board i j str
  | j-1 >= 0 && (board !! i) !! (j-1) == head str = False
  | i-1 >= 0 && (board !! (i-1)) !! j == head str = False
  | j+1 < length (head board) && (board !! i) !! (j+1) == head str = False
  | otherwise = True

checkSpecs :: [String] -> [Int] -> [Int] -> [Int] -> [Int] -> Int -> Int -> Bool
checkSpecs board left right top bot i j =
  let
    posCountHor = replicate (length board) 0
    negCountHor = replicate (length board) 0
    posCountVer = replicate (length (head board)) 0
    negCountVer = replicate (length (head board)) 0
  in
    checkSpecs2 board left right top bot posCountHor negCountHor posCountVer negCountVer i j

checkSpecs2 :: [String] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Int -> Int -> Bool
checkSpecs2 board left right top bot posH negH posV negV i j =
  let
    (newPosH, newNegH) = hLoop board i j posH negH
    (newPosV, newNegV) = vLoop board i j posV negV
  in
    checkH left right newPosH newNegH 0 False && checkV top bot newPosV newNegV 0 False


checkH :: [Int] -> [Int] -> [Int] -> [Int] -> Int -> Bool -> Bool
checkH left right posH negH index isRepeat
  | index >= length posH = True
  | left !! index /= -1 && not isRepeat =
    if posH !! index /= (left !! index) then
      False
    else
      checkH left right posH negH index True
  | right !! index /= -1 && isRepeat =
    if negH !! index /= (right !! index) then
      False
    else
      checkH left right posH negH (index+1) False
  | otherwise = checkH left right posH negH (index+1) False

checkV :: [Int] -> [Int] -> [Int] -> [Int] -> Int -> Bool -> Bool
checkV top bot posV negV index isRepeat
  | index >= length posV = True
  | top !! index /= -1 && not isRepeat =
    if posV !! index /= (top !! index) then
      False
    else
      checkV top bot posV negV index True
  | bot !! index /= -1 && isRepeat =
    if negV !! index /= (bot !! index) then
      False
    else
      checkV top bot posV negV (index+1) False
  | otherwise = checkV top bot posV negV (index+1) False


hLoop :: [String] -> Int -> Int -> [Int] -> [Int] -> ([Int], [Int])
hLoop [] _ _ pos neg = (pos, neg)
hLoop (row:rows) i j pos neg =
  -- Get the j-th character in the row (safe version)
  let cell = if j < length row then row !! j else ' ' 
  in case cell of
    '+' -> hLoop rows (i+1) j (mutateList pos i) neg
    '-' -> hLoop rows (i+1) j pos (mutateList neg i)
    _   -> hLoop rows (i+1) j pos neg

vLoop :: [String] -> Int -> Int -> [Int] -> [Int] -> ([Int], [Int])
vLoop board i j pos neg =
  foldl
    (\(p, n) row ->
        let cell = if j < length row then row !! j else ' '
        in case cell of
            '+' -> (mutateList p j, n)
            '-' -> (p, mutateList n j)
            _   -> (p, n))
    (pos, neg)
    board



mutateList :: [Int] -> Int -> [Int]
mutateList old index =
  take index old ++ [(old !! index) + 1] ++ drop (index+1) old


mutateBoard :: [String] -> Int -> Int -> Char -> [String]
mutateBoard board i j newVal =
  take i board ++
  [mutateRow (board !! i) j newVal] ++
  drop (i+1) board
  where
    mutateRow :: String -> Int -> Char -> String
    mutateRow str col newVal =
      take col str ++ [newVal] ++ drop (col+1) str


-- if i == length board && j == 0 then
solvePuzzle :: [String] -> [Int] -> [Int] -> [Int] -> [Int] -> Int -> Int -> Maybe [String]
solvePuzzle board left right top bot i j
  | i >= length board && j == 0 && checkSpecs board left right top bot 0 0 = Just board
  | j >= length (head board) = solvePuzzle board left right top bot (i+1) 0
  | i >= length board = Nothing
  | otherwise =

    -- Check for Horizontal Placements
    if (board !! i) !! j == 'L' then
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
            newboard2 = mutateBoard newboard i (j+1) '+'
            in
              solvePuzzle newboard2 left right top bot i (j+2)
        else
          -- XX Condition j=x, j+1=x, j=l,j+1=R, 
          if canPutHorizontal board i j "XX" then
            let
              newboard = mutateBoard board i j 'X'
              newboard2 = mutateBoard newboard i (j+1) 'X'
              in
                solvePuzzle newboard2 left right top bot i (j+2)
          else
            solvePuzzle board left right top bot i (j+1)

    -- To deal with Vertical Orientations
    else
      solveHelper board left right top bot i j
      


solveHelper :: [String] -> [Int] -> [Int] -> [Int] -> [Int] -> Int -> Int -> Maybe [String]
solveHelper board left right top bot i j
  | (board !! i) !! j == 'T' =
    -- +- Condition
      if canPutVertical board i j "+-" then
        let
          newboard = mutateBoard board i j '+'
          newboard2 = mutateBoard newboard (i+1) j '-'
        in
          solvePuzzle newboard2 left right top bot i (j+1)
      else
        -- -+ Condition 
        if canPutVertical board i j "-+" then
          let
            newboard = mutateBoard board i j '-'
            newboard2 = mutateBoard newboard (i+1) j '+'
          in
            solvePuzzle newboard2 left right top bot i (j+1)
        else
          -- XX Condition
          if canPutVertical board i j "XX" then
            let
              newboard = mutateBoard board i j 'X'
              newboard2 = mutateBoard newboard (i+1) j 'X'
            in
              solvePuzzle newboard2 left right top bot i (j+1)
          else
            solvePuzzle board left right top bot i (j+1)
  | otherwise = solvePuzzle board left right top bot i (j+1)

  -- [ "+-+-X-" , "-+-+X+", "XX+-+-", "XX-+X+", "-+XXX-" ]
