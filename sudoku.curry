{-# OPTIONS_CYMAKE -Wnone #-}
{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}

import Sort
import List

{-
    4x4 sudoku solver written in the functional logic programming language Curry.
    Tim Coutinho
-}

type Sudoku = [[Int]]

-- Utility function that gives either the first two or last two elements of a list
set :: Int -> [e] -> [e]
set 0 [a,b,_,_] = [a,b]
set 1 [_,_,c,d] = [c,d]


-- Transposes the board
transpose :: Sudoku -> Sudoku
transpose ([]:_) = []
transpose xs = map head xs : transpose (map tail xs)

-- Gives the rth row in the board
row :: Int -> Sudoku -> [Int]
row r b = b !! r

-- Gives the rth column in the board
col :: Int -> Sudoku -> [Int]
col c b = row c $ transpose b
--col c b = map (!! c) b

-- Gives the box at position n in the board, where 1,2,3,4 correspond
-- to top left, top right, bottom left, and bottom right of the board, respectively
box :: Int -> Sudoku -> [Int]
box n b = concat $ map (set r) (set c b)
 where r = n `div` 2-- | 0 | 1 |
       c = n `mod` 2

board1 = [[1,2,3,4],
          [2,3,4,5],
          [3,4,5,6],
          [4,5,6,7]]

board2 = [[1,2,3,4],
          [3,4,2,1],
          [2,1,4,3],
          [4,3,1,2]]

-- Determines if a row, column, or box (list of ints) has no duplicates
noDups :: [Int] -> Bool
noDups [] = True
noDups (x:xs)
 | x `elem` xs = False
 | otherwise   = noDups xs

-- Determines if a sudoku board is valid (no duplicates in each row, column, and box)
valid :: Sudoku -> Sudoku
valid b
 | and [rows, cols, boxes] = b
 where rows  = and $ map noDups b
       cols  = and $ map noDups $ transpose b
       boxes = and $ map noDups [box n b | n <- [0..3]]

-- Example boards
s1 = "1234\n" ++
     "3412\n" ++
     " 1 3\n" ++
     " 3 1"

s2 = "1 3 \n" ++
     " 4 2\n" ++
     "2 4 \n" ++
     " 3 1"

s3 = "1   \n" ++
     "    \n" ++
     "    \n" ++
     " 3  "

toInt :: Char -> Int
toInt '1' = 1
toInt '2' = 2
toInt '3' = 3
toInt '4' = 4
toInt ' ' = 1
toInt ' ' = 2
toInt ' ' = 3
toInt ' ' = 4

-- Converts a string into a sudoku board
toBoard :: String -> Sudoku
toBoard s = [map toInt n | n <- lines s]

-- Solves the sudoku board by checking if a certain board is valid
solver :: String -> Sudoku
solver = valid . toBoard
