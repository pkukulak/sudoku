module Sudoku where

import Data.Char
import Test.QuickCheck
import Data.List
import Data.Maybe
import Data.List.Split
import System.Random

-- |The Sudoku datatype.
data Sudoku = Sudoku [[Maybe Int]]
    deriving (Show, Eq)

-- |The Block alias.
type Block = [Maybe Int]
type Pos = (Int, Int)

{- ########### CODE TO ##########
   ######## PRINT SUDOKUS ####### -}
-- |Prints a Sudoku puzzle.
printSudoku :: Sudoku -> IO ()
printSudoku s =
    let     sudokuStr = sudokuToString $ rows s
    in      mapM_ putStrLn $ init sudokuStr

-- |Converts a Sudoku into a list of Strings.
sudokuToString :: [[Maybe Int]] -> [String]
sudokuToString [] = [[]]
sudokuToString (r:rs) =
    maybeToString r : sudokuToString rs

-- |Converts a list of Maybe Ints to a String.
--  Could probably be written in a cleaner way.
maybeToString :: [Maybe Int] -> String
maybeToString [] = []
maybeToString (x:xs) =
    case x of
        Just n -> (show n) ++ maybeToString xs
        Nothing -> '.' : maybeToString xs
{- ################################# -}

{- ########## CODE TO ###########
   ####### READ SUDOKUS ######### -}
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
linesToSudoku :: [String] -> [[Maybe Int]]
linesToSudoku [] = [[]]
linesToSudoku (l:ls) =
    sudokuLine l : linesToSudoku ls

-- |Reads a single String representing a single
--  row of a Sudoku puzzle, converting it to a list
--  Maybe Int.
sudokuLine :: String -> [Maybe Int]
sudokuLine [] = []
sudokuLine (x:xs) =
    case x of
        '.' -> Nothing : sudokuLine xs
        c -> Just (digitToInt c) : sudokuLine xs
{- ################################## -}
        
-- |Extract rows from a given Sudoku.
rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

-- |Extract columns from a given Sudoku.
columns :: Sudoku -> [[Maybe Int]]
columns (Sudoku s) = 
    let     rs = rows (Sudoku s)
    in      [map (head . drop i) rs | i <- [0..8]]

-- |Extract 3x3 blocks from a given Sudoku.
threeByThrees :: Sudoku -> [[Maybe Int]]
threeByThrees (Sudoku s) =
    let     rs = rows (Sudoku s)
            blx = [map (take 3 . drop i) rs | i <- init [0,3..9]]
            segmentedBlx = concat blx
    in      combineBlocks segmentedBlx

combineBlocks :: [[a]] -> [[a]]
combineBlocks [] = []
combineBlocks blk =
    (concat $ take 3 blk) : (combineBlocks $ drop 3 blk)

example :: Sudoku
example = Sudoku
      [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
      , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
      , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
      , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
      , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
      , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
      , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
      ]

invalidExample :: Sudoku
invalidExample = Sudoku
    [ [Just 3, Just 6, Nothing]
    , [Nothing, Nothing]
    ]

-- |A Sudoku containing only blank cells.
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (allBlankSudokuHelper 9)

-- |Helper for generating blank Sudokus.
allBlankSudokuHelper :: Int -> [[Maybe Int]]
allBlankSudokuHelper 1 = allBlankRow
allBlankSudokuHelper n = allBlankRow ++ allBlankSudokuHelper (n - 1)

-- |A row containing all blank cells.
allBlankRow :: [[Maybe Int]]
allBlankRow = [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]]

-- |Returns True if and only if the input is a valid Sudoku puzzle.
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku s) =
    let     rs = rows (Sudoku s)
            numRows = length rs
    in      numRows == 9
         && numberOfEntries rs == 81
         && allValidEntries rs
isSudoku _ = False

-- |Calculates the number of entries in the Sudoku.
numberOfEntries :: [[Maybe Int]] -> Int
numberOfEntries (r:rs) = length r + numberOfEntries rs
numberOfEntries [] = 0

-- |Returns True if and only if all entries in the Sudoku are valid.
allValidEntries :: [[Maybe Int]] -> Bool
allValidEntries (r:[]) = validRow r
allValidEntries (r:rs) = validRow r && allValidEntries rs

-- |Check if a row in the given Sudoku is valid
validRow :: [Maybe Int] -> Bool
validRow (x:[]) =
    case x of
        Just _ -> True
        Nothing -> True
        _ -> False
validRow (x:xs) =
    case x of
        Just _ -> True && validRow xs
        Nothing -> True && validRow xs
        _ -> False

-- |Returns True if and only if the Sudoku is solved.
isSolved :: Sudoku -> Bool
isSolved (Sudoku s) =
    noNothings (rows (Sudoku s))

-- |Returns True if and only if there are no Nothings in the Sudoku.
noNothings :: [[Maybe Int]] -> Bool
noNothings (r:[]) = noNothingsRow r
noNothings (r:rs) = noNothingsRow r && noNothings rs

-- |Returns True if and only if every entry in the row is not Nothing
noNothingsRow :: [Maybe Int] -> Bool
noNothingsRow (x:[]) =
    case x of
        Just _ -> True
        Nothing -> False
noNothingsRow (x:xs) =
    case x of
        Just _ -> True && noNothingsRow xs
        Nothing -> False

-- |Returns True if and only if the given Block is valid.
isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (b:bs) =
    case b of
        Just x -> not $ b `elem` bs
        Nothing -> isOkayBlock bs

-- |Returns all Blocks of the given Sudoku.
--  That is, will return all rows, columns and 3x3 blocks.
blocks :: Sudoku -> [Block]
blocks (Sudoku s) = rows (Sudoku s) 
                    ++ columns (Sudoku s)
                    ++ threeByThrees (Sudoku s)

-- |Returns True if and only if the given Sudoku is solvable.
isOkay :: Sudoku -> Bool
isOkay (Sudoku s) =
    let     allBlocks = map isOkayBlock (blocks (Sudoku s))
    in      not (False `elem` allBlocks)

-- |Returns the first position in the given Sudoku
--  at which occurs a Nothing.
blank :: Sudoku -> Maybe Pos
blank (Sudoku s) =
    let     rs = rows (Sudoku s)
            blanks = allBlanks rs
    in      case blanks of
        [] -> Nothing
        _ -> Just $ head blanks
    
    
-- |Returns all blank positions in the given Sudoku.
allBlanks :: [[Maybe Int]] -> [Pos]
allBlanks rs =
    let     indices = [(i, j) | i <- [0..8], j <- [0..8]]
    in      filter (\pos -> isBlank pos rs) indices

-- |Returns True if and only if the position in the given
--  Sudoku is blank
isBlank :: Pos -> [[Maybe Int]] -> Bool
isBlank (i, j) rs =
    (rs !! i !! j) == Nothing

-- |Returns a new list with element e at the nth
--  position in lst.
(!!=) :: [a] -> (Int, a) -> [a]
lst !!= (n, e) =
    let     left = take n lst
            right = drop (n + 1) lst
    in      left ++ [e] ++ right

-- |Returns a new Sudoku with the position (x, y)
--  updated to contain n.
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku s) (x, y) n =
    let     rs = rows (Sudoku s)
            upper = take x rs
            lower = drop (x + 1) rs
            updated = (rs !! x) !!= (y, n)
    in      Sudoku (upper ++ [updated] ++ lower)

-- |Returns True if and only if the given Sudoku
--  contains n at the given position (x, y).
isUpdated :: Sudoku -> Pos -> Maybe Int -> Bool
isUpdated (Sudoku s) (x, y) n =
    let     rs = rows (Sudoku s)
    in      (rs !! x !! y) == n

solve :: Sudoku -> Maybe Sudoku
solve sud = undefined
