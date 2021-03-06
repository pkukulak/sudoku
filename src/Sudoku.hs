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

-- |Solves a given Sudoku. Returns Nothing if there is
--  no solution.
solve :: Sudoku -> Maybe Sudoku
solve sud = 
    case isOkay sud of
        False   -> Nothing
        True    -> 
            case blank sud of
                Nothing     -> Just sud
                Just (x, y) ->
                    let     toSolve = update sud (x, y)
                            allPossible = map toSolve [Just n | n <- [1..9]]
                            solveAll = map solve allPossible
                    in      firstSolution solveAll

-- |Returns the first solution in a list of all possible
--  solutions to a Sudoku.                        
firstSolution :: [Maybe Sudoku] -> Maybe Sudoku
firstSolution [] = Nothing
firstSolution (x:xs) =
    case x of
        Nothing     -> firstSolution xs
        _           -> x
      
-- |Extract rows from a given Sudoku.
rows :: Sudoku -> [Block]
rows (Sudoku rs) = rs

-- |Extract columns from a given Sudoku.
columns :: Sudoku -> [Block]
columns (Sudoku s) = 
    let     rs = rows (Sudoku s)
    in      [map (head . drop i) rs | i <- [0..8]]

-- |Extract 3x3 blocks from a given Sudoku.
threeByThrees :: Sudoku -> [Block]
threeByThrees (Sudoku s) =
    let     rs = rows (Sudoku s)
            blx = [map (take 3 . drop i) rs | i <- init [0,3..9]]
            segmentedBlx = concat blx
    in      combineBlocks segmentedBlx

-- |A helper function used to combine 3x3 segments of lists of lists.
combineBlocks :: [[a]] -> [[a]]
combineBlocks [] = []
combineBlocks blk =
    (concat $ take 3 blk) : (combineBlocks $ drop 3 blk)

-- |A Sudoku containing only blank cells.
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (allBlankSudokuHelper 9)

-- |Helper for generating blank Sudokus.
allBlankSudokuHelper :: Int -> [Block]
allBlankSudokuHelper 1 = allBlankRow
allBlankSudokuHelper n = allBlankRow ++ allBlankSudokuHelper (n - 1)

-- |A row containing all blank cells.
allBlankRow :: [Block]
allBlankRow = [replicate 9 Nothing]


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
numberOfEntries :: [Block] -> Int
numberOfEntries (r:rs) = length r + numberOfEntries rs
numberOfEntries [] = 0

-- |Returns True if and only if all entries in the Sudoku are valid.
allValidEntries :: [Block] -> Bool
allValidEntries (r:[]) = validRow r
allValidEntries (r:rs) = validRow r && allValidEntries rs

-- |Check if a row in the given Sudoku is valid
validRow :: Block -> Bool
validRow (x:[]) =
    case x of
        Just _  -> True
        Nothing -> True
        _       -> False
validRow (x:xs) =
    case x of
        Just _  -> True && validRow xs
        Nothing -> True && validRow xs
        _       -> False

-- |Returns True if and only if the Sudoku is solved.
isSolved :: Sudoku -> Bool
isSolved (Sudoku s) =
    noNothings (rows (Sudoku s))

-- |Returns True if and only if there are no Nothings in the Sudoku.
noNothings :: [Block] -> Bool
noNothings (r:[]) = noNothingsRow r
noNothings (r:rs) = noNothingsRow r && noNothings rs

-- |Returns True if and only if every entry in the row is not Nothing
noNothingsRow :: Block -> Bool
noNothingsRow (x:[]) =
    case x of
        Just _  -> True
        Nothing -> False
noNothingsRow (x:xs) =
    case x of
        Just _  -> True && noNothingsRow xs
        Nothing -> False

-- |Returns True if and only if the given Block is valid.
isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (b:bs) =
    case b of
        Just x  -> (not $ b `elem` bs) && isOkayBlock bs
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
    in      listToMaybe blanks
    
-- |Returns all blank positions in the given Sudoku.
allBlanks :: [Block] -> [Pos]
allBlanks rs =
    let     indices = [(i, j) | i <- [0..8], j <- [0..8]]
    in      filter (\pos -> isBlank pos rs) indices

-- |Returns True if and only if the position in the given
--  Sudoku is blank
isBlank :: Pos -> [Block] -> Bool
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

