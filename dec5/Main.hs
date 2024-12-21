import System.IO
import Data.List (isPrefixOf, stripPrefix, sortBy)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Debug.Trace (trace)

-- Function to parse ordering rules
parseOrderingRules :: [String] -> [(Int, Int)]
parseOrderingRules = map (\line -> let [a, b] = split '|' line in (read a, read b))

-- Function to parse lists of integers
parseLists :: [String] -> [[Int]]
parseLists = map (map read . split ',')

-- Helper function to split a string by a delimiter
split :: Char -> String -> [String]
split delim = foldr (\c (x:xs) -> if c == delim then []:x:xs else (c:x):xs) [[]]

-- Function to create a hashmap from ordering rules
createHashMap :: [(Int, Int)] -> Map.Map Int [Int]
createHashMap = foldl insertRule Map.empty
  where
    insertRule acc (a, b) = Map.insertWith (++) a [b] acc

-- given a hashmap of ordering rules and a list of integers, if the list is ordered according to the rules of the hashmap, then return the middle element, otherwise return 0
getMiddleElementIfSorted orderMap numbs = go [] numbs
    where
        go numbersSoFar numbs'
            | null numbs' = trace ("Valid middle value: " ++ show middleValue) middleValue
            | otherwise = let
                nextNumber = head numbs'
                nextNumbers = Map.findWithDefault [] nextNumber orderMap
                in if any (`elem` numbersSoFar) nextNumbers
                    then trace ("Invalid number found: " ++ show nextNumber) 0
                    else go (nextNumber:numbersSoFar) (tail numbs')
        middleIndex = length numbs `div` 2
        middleValue = numbs !! middleIndex


solvePart1 :: Map.Map Int [Int] -> [[Int]] -> Int
solvePart1 = go
    -- basically just sum the results of getMiddleElementIfSorted for each list
    where
        go orderMap [] = 0
        go orderMap (x:xs) = getMiddleElementIfSorted orderMap x + go orderMap xs

-- Comparison function for sortBy
compareByOrder :: Map.Map Int [Int] -> Int -> Int -> Ordering
compareByOrder orderMap a b
    | a == b = EQ
    | b `elem` Map.findWithDefault [] a orderMap = LT
    | a `elem` Map.findWithDefault [] b orderMap = GT
    | otherwise = EQ

-- Instead of summing the middle values of the sorted lists we first sort the lists according to the ordering rules and then sum the middle values
solvePart2 :: Map.Map Int [Int] -> [[Int]] -> Int
solvePart2 orderMap lists = sum $ map processList lists
  where
    processList list =
        let sorted = sortBy (compareByOrder orderMap) list
        in trace ("Original: " ++ show list ++ ", Sorted: " ++ show sorted) $
           getMiddleElementIfSorted orderMap sorted


main = do
    fileContent <- readFile "./dec5/input.txt"
    let linesContent = lines fileContent
        (orderingRulesLines, listsLines) = break null linesContent
        orderingRules = parseOrderingRules orderingRulesLines
        lists = parseLists (tail listsLines) -- tail to skip the empty line
        orderMap = createHashMap orderingRules
    print $ solvePart1 orderMap lists
