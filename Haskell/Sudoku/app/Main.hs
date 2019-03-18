module Main where

import Operators
import SudokuParser
import Board
import SudokuSolver
import Text.Parsec
import System.IO

hardPath :: FilePath
hardPath = "D:\\GitHub\\Sudoku\\input.txt"

main :: IO ()
main = do
  putStrLn "Please, enter path to sudoku file or blank line for default"
  line <- getLine
  let path = if null line then hardPath else line
  withFile path ReadMode (handleFile path)

handleFile :: FilePath -> Handle -> IO ()
handleFile path handle = do
  contents <- hGetContents handle
  runParser sudokuParser () path contents |> either (\err -> putStrLn <| "File format error:\n" ++ show err) handleSudoku

handleSudoku :: Board (Maybe Int) -> IO ()
handleSudoku board = solveSudoku board |> maybe (putStrLn "Could not solve") prettyPrint
