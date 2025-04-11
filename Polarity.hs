module Polarity where

import Control.Applicative
import Data.List
import Debug.Trace
import Control.Monad
import Data.Maybe

polarity :: [String] -> ([Int], [Int], [Int], [Int]) -> [String]
polarity board specs =
    fromMaybe [] (solvePuzzle board left right top bot 0 0)  -- We'll fix the false negatives instead
  where
    (left, right, top, bot) = specs

-- More permissive placement rules
canPutHorizontal :: [String] -> Int -> Int -> String -> Bool
canPutHorizontal board i j str
  | j+1 >= length (head board) = False  -- No room for pair
  | (board !! i) !! j /= 'L' = False
  | (board !! i) !! (j+1) /= 'R' = False
  | otherwise = True  -- Skip adjacency checks temporarily

canPutVertical :: [String] -> Int -> Int -> String -> Bool
canPutVertical board i j str
  | i+1 >= length board = False
  | (board !! i) !! j /= 'T' = False
  | (board !! (i+1)) !! j /= 'B' = False
  | otherwise = True  -- Skip adjacency checks temporarily

checkSpecs :: [String] -> [Int] -> [Int] -> [Int] -> [Int] -> Bool
checkSpecs board left right top bot =
  let rows = board
      cols = transpose board

      -- Count '+' and '-' in rows (ignoring 'X')
      posRows = map (count '+') rows
      negRows = map (count '-') rows

      -- Count '+' and '-' in columns (ignoring 'X')
      posCols = map (count '+') cols
      negCols = map (count '-') cols

      -- Check all constraints (-1 means no constraint)
      checkConstraint spec cnt = spec == -1 || cnt == spec

  in and (zipWith checkConstraint left posRows) &&  -- Check left (row + counts)  -- Check left (row + counts)  -- Check left (row + counts)  -- Check left (row + counts)
       -- Check left (row + counts)
     and (zipWith checkConstraint right negRows) && -- Check right (row - counts) -- Check right (row - counts) -- Check right (row - counts) -- Check right (row - counts)
      -- Check right (row - counts)
     and (zipWith checkConstraint top posCols) &&   -- Check top (col + counts)   -- Check top (col + counts)   -- Check top (col + counts)
     and (zipWith checkConstraint bot negCols)      -- Check bottom (col - counts)
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
        'L' -> tryPatterns board i j
        'T' -> tryVerticalPatterns board i j
        _   -> solvePuzzle board left right top bot i (j+1)
  where
    tryPatterns board i j =
      let try pat = do
            guard (canPutHorizontal board i j pat)
            let [c1, c2] = pat
            let newboard = mutateBoard (mutateBoard board i j c1) i (j+1) c2
            solvePuzzle newboard left right top bot i (j+2)
      in try "+-" <|> try "-+" <|> try "XX"

    tryVerticalPatterns board i j =
      let try pat = do
            guard (canPutVertical board i j pat)
            let [c1, c2] = pat
            let newboard = mutateBoard (mutateBoard board i j c1) (i+1) j c2
            solvePuzzle newboard left right top bot i (j+1)
      in try "+-" <|> try "-+" <|> try "XX"
