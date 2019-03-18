module SudokuSolver ( solveSudoku ) where

{- Index naming conventions
 - base indices:        P[i, j, k]
 - alternative indices: P[m, n, t]
 -}

import Operators
import Board
import Variable
import Data.Boolean.SatSolver
import Control.Monad
import Data.List.Index
import Data.List
import Data.Maybe

solveSudoku :: Board (Maybe Int) -> Maybe (Board Int)
solveSudoku sudokuBoard = createSudokuSolver sudokuBoard |> listToMaybe >>= extractSolution

createSudokuSolver :: Board (Maybe Int) -> [SatSolver]
createSudokuSolver (Board sudokuBoard) = ifoldl addLineConstraints newSudokuSolver sudokuBoard >>= solve where
  addLineConstraints :: [SatSolver] -> Int -> [Maybe Int] -> [SatSolver]
  addLineConstraints solvers i line = ifoldl (addElementConstraints i) solvers line
  -- i index is passed as first argument for historical reasons
  addElementConstraints :: Int -> [SatSolver] -> Int -> Maybe Int -> [SatSolver]
  addElementConstraints i solvers j mk = maybe solvers ((solvers >>=) . assertTrue . var i j) mk

{- Solver with standard sudoku constraints -}
newSudokuSolver :: MonadPlus m => m SatSolver
newSudokuSolver = foldr ((=<<) . assertTrue) (return newSatSolver) allConstraints where
  -- a cell can only contain one value
  constraints1 = [ (var i j k) ==> (Not <| var i j t) | i <- board, j <- board, k <- domain, t <- domain, t /= k ]
  -- a cell has to contain a value
  constraints2 = [ foldl1 (:||:) [ var i j k | k <- domain ] | i <- board, j <- board ]
  -- all values in line should be distinct
  constraints3 = [ (var i j k) ==> (Not <| var i n k) | i <- board, j <- board, n <- board, j /= n, k <- domain ]
  -- all values in column should be distinct
  constraints4 = [ (var i j k) ==> (Not <| var m j k) | i <- board, j <- board, m <- board, i /= m, k <- domain ]
  -- all values in square should be distinct
  constraints5 = [ (var (3 * r + i) (3 * s + j) k) ==> (Not <| var (3 * r + m) (3 * s + n) k)
    | r <- [0..2], s <- [0..2], i <- [0..2], j <- [0..2], k <- domain, m <- [0..2], n <- [0..2], (m, n) /= (i, j) ]
  allConstraints = concat [ constraints1, constraints2, constraints3, constraints4, constraints5 ]

extractSolution :: SatSolver -> Maybe (Board Int)
extractSolution solver = sequence [ sequence [ extractValue solver i j | j <- board ] | i <- board ] |> fmap Board

extractValue :: SatSolver -> Int -> Int -> Maybe Int
extractValue solver i j = [ varValue solver <| Variable i j k | k <- domain ] |> elemIndex True |> fmap succ

varValue :: SatSolver -> Variable -> Bool
varValue solver (Variable i j k) = lookupVar (Variable i j k |> uniqueCode) solver |> maybe False id

board :: [Int]
board = [0..8]

domain :: [Int]
domain = [1..9]

(==>) :: Boolean -> Boolean -> Boolean
a ==> b = (Not a) :||: b

var :: Int -> Int -> Int -> Boolean
var i j k = Variable i j k |> uniqueCode |> Var
