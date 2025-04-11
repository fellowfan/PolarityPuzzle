module Polarity where

import Data.Array
import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe

type Grid = Array (Int, Int) Char

polarity :: [String] -> ([Int], [Int], [Int], [Int]) -> [String]
polarity board specs =
    fromMaybe [] (solvePuzzle board left right top bot 0 0)
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

checkSpecs :: [String] -> [Int] -> [Int] -> [Int] -> [Int] -> Bool
checkSpecs board left right top bot =
  let rows = board
      cols = transpose board

      -- Count rows
      posRows = map (count '+') rows
      negRows = map (count '-') rows

      -- Count columns
      posCols = map (count '+') cols
      negCols = map (count '-') cols

      checkConstraint spec cnt = spec == -1 || cnt == spec

  in and (zipWith checkConstraint left posRows) &&
     and (zipWith checkConstraint right negRows) &&
     and (zipWith checkConstraint top posCols) &&
     and (zipWith checkConstraint bot negCols)
  where
    count c = length . filter (== c)

mutateBoard :: [String] -> Int -> Int -> Char -> [String]
mutateBoard board i j newVal =
  take i board ++
  [mutateRow (board !! i) j newVal] ++
  drop (i+1) board
  where
    mutateRow :: String -> Int -> Char -> String
    mutateRow str col newVal =
      take col str ++ [newVal] ++ drop (col+1) str


solvePuzzle :: [String] -> [Int] -> [Int] -> [Int] -> [Int] -> Int -> Int -> Maybe [String]
solvePuzzle board left right top bot i j
  | i >= length board && j == 0 =
      if checkSpecs board left right top bot
      then Just board
      else Nothing
  | j >= length (head board) = solvePuzzle board left right top bot (i+1) 0
  | otherwise =
      case (board !! i) !! j of
        'L' -> tryAllPatterns board i j
        'T' -> tryAllVerticalPatterns board i j
        _   -> solvePuzzle board left right top bot i (j+1)
  where
    tryAllPatterns board i j =
      tryPattern "+-" <|> tryPattern "-+" <|> tryPattern "XX"
      where
        tryPattern pat = do
          guard (canPutHorizontal board i j pat)
          let [c1, c2] = pat
          let newboard = mutateBoard (mutateBoard board i j c1) i (j+1) c2
          solvePuzzle newboard left right top bot i (j+2)

    tryAllVerticalPatterns board i j =
      tryPattern "+-" <|> tryPattern "-+" <|> tryPattern "XX"
      where
        tryPattern pat = do
          guard (canPutVertical board i j pat)
          let [c1, c2] = pat
          let newboard = mutateBoard (mutateBoard board i j c1) (i+1) j c2
          solvePuzzle newboard left right top bot i (j+1)

    checkAndContinue newboard nextJ =
      if checkSpecs newboard left right top bot
      then solvePuzzle newboard left right top bot i nextJ
      else Nothing
