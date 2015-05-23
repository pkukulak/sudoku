module Main where

import Sudoku
import System.Environment
import Data.Char
import Data.Maybe

main = do
    putStrLn "Enter the name of a Sudoku file: "
    path <- getLine
    sud <- readSudoku path
    putStrLn "This is the Sudoku you gave as input: "
    printSudoku sud
    putStrLn "This is the solution: "
    let     solution = fromJust $ solve sud
    printSudoku solution

-- |Reads a Sudoku puzzle from an input file.
readSudoku :: FilePath -> IO Sudoku
readSudoku path =
    do
        content <- readFile path
        let     linesOfFile = lines content
        return $ Sudoku (init (linesToSudoku linesOfFile))

-- |Reads a list of Strings representing all rows
--  of a Sudoku puzzle and converts it into a list of
--  list of Maybe Int.
linesToSudoku :: [String] -> [Block]
linesToSudoku [] = [[]]
linesToSudoku (l:ls) =
    sudokuLine l : linesToSudoku ls

-- |Reads a single String representing a single
--  row of a Sudoku puzzle, converting it to a list
--  Maybe Int.
sudokuLine :: String -> Block
sudokuLine [] = []
sudokuLine (x:xs) =
    case x of
        '.' -> Nothing : sudokuLine xs
        c -> Just (digitToInt c) : sudokuLine xs

-- |Prints a Sudoku puzzle.
printSudoku :: Sudoku -> IO ()
printSudoku s =
    let     sudokuStr = sudokuToString $ rows s
    in      mapM_ putStrLn $ init sudokuStr

-- |Converts a Sudoku into a list of Strings.
sudokuToString :: [Block] -> [String]
sudokuToString [] = [[]]
sudokuToString (r:rs) =
    maybeToString r : sudokuToString rs

-- |Converts a list of Maybe Ints to a String.
--  Could probably be written in a cleaner way.
maybeToString :: Block -> String
maybeToString [] = []
maybeToString (x:xs) =
    case x of
        Just n  -> (show n) ++ " " ++ maybeToString xs
        Nothing -> ". " ++ maybeToString xs
