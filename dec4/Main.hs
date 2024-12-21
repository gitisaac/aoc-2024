import System.IO
import Control.Monad
import Data.Function ((&))
import Data.List (nub, transpose)
import Debug.Trace ( trace )

main = do
    fileContent <- readFile "./dec4/input.txt"
    let matrix = fileContent & lines & map (map (:[]))
    --print $ solvePart1 matrix
    print $ solvePart2 matrix


countWord :: [String] -> (Int, Int) -> (Int, Int) -> [[String]] -> Int
countWord words start delta matrix = go start 0
    where
        go (x, y) count
            | x < 0 || x >= dimX = count
            | y < 0 || y >= dimY = count
            | otherwise = let
                word = wordAhead (x, y) delta (maximum $ map length words) matrix
                newCount = if word `elem` words then count + 1 else count
                in go (plusCoord (x, y) delta) newCount
        dimX = length matrix
        dimY = if null matrix then 0 else length (head matrix)
        plusCoord (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)

wordAhead :: (Int, Int) -> (Int, Int) -> Int -> [[String]] -> String
wordAhead start delta wordLength matrix = go start ""
    where
        dimY = case matrix of
            [] -> 0
            (firstRow:_) -> length firstRow
        dimX = length matrix
        plusCoord (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)
        go (x, y) word
            | x < 0 || x >= dimX = word
            | y < 0 || y >= dimY = word
            | length word >= wordLength = word
            | otherwise = go (plusCoord (x, y) delta) (word ++ ((matrix !! x) !! y))



-- The correct answer is 2554
solvePart1 :: [[String]] -> Int
solvePart1 matrix =
    let
        words = ["XMAS", "SAMX"]
        dimX = length matrix
        dimY = case matrix of
            [] -> 0
            (x:_) -> length x
        rowPositions = [(x, 0) | x <- [0..dimX - 1]]
        colPositions = [(0, y) | y <- [0..dimY - 1]]
        diagPositionsTopLeft = nub $ rowPositions ++ colPositions
        diagPositionsTopRight = nub $ [(x, dimY - 1) | x <- [0..dimX - 1]] ++ [(0, y) | y <- [0..dimY - 1]]
        allPositions = [(pos, (0, 1)) | pos <- rowPositions] ++
                       [(pos, (1, 0)) | pos <- colPositions] ++
                       [(pos, (1, 1)) | pos <- diagPositionsTopLeft] ++
                       [(pos, (1, -1)) | pos <- diagPositionsTopRight]
    in sum [let count = countWord words pos delta matrix 
            in trace (show pos ++ " " ++ show delta ++ " " ++ show count) count 
            | (pos, delta) <- allPositions]

countCrossedWord :: [String] -> (Int, Int) -> [[String]] -> Int
countCrossedWord words start matrix = go start 0
    where
        go (x, y) count
            | x < 0 || x >= dimX = count
            | y < 0 || y >= dimY = count
            | otherwise = let
                word = wordAhead (x, y) (1, 1) wordLen matrix
                wordCrossed = wordAhead (x, y + wordLen - 1) (1, -1) wordLen matrix
                newCount = if word `elem` words && wordCrossed `elem` words then count + 1 else count
                traceMsg = "word: " ++ word ++ ", wordCrossed: " ++ wordCrossed ++ ", newCount: " ++ show newCount
                in trace traceMsg $ go (x, y + 1) newCount
        wordLen = maximum $ map length words
        dimX = length matrix
        dimY = if null matrix then 0 else length (head matrix)

solvePart2 :: [[String]] -> Int
solvePart2 matrix =
    let
        words = ["MAS", "SAM"]
        dimX = length matrix
        dimY = case matrix of
            [] -> 0
            (x:_) -> length x
        rowPositions = [(x, 0) | x <- [0..dimX - 1]]
        allPositions = [(pos, (0, 1)) | pos <- rowPositions]
    in sum [let count = countCrossedWord words pos matrix 
            in trace (show pos ++ " " ++ show count) count 
            | (pos, _) <- allPositions]
