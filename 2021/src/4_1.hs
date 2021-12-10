import Data.List
import System.Environment 
import Data.List.Split (splitOn)
import Data.Maybe

-- solving

boardScore :: [[Int]] -> Int
boardScore = sum . map pred . filter (>0) . concat

checkBoard :: [[Int]] -> Bool
checkBoard b = any (all (< 0)) (b++bT) 
  where bT = transpose b

check :: [[[Int]]] -> Maybe [[Int]]
check  = find checkBoard

play :: Int -> [[[Int]]] -> [[[Int]]] 
play n = map $ map $ map (\x -> if x == n then -x else x)

solve :: ([Int], [[[Int]]]) -> Int
solve ( h:t ,bs) = maybe (solve (t,bs')) (((h-1) *) . boardScore) 
                 $ check bs'
    where bs' = play h bs


-- increased the number by 1.
-- Now, a marked number is a negative number
-- Note: before scoring subtract 1 to all the number in the board.- parsing



parseNumbers :: String -> [Int]
parseNumbers = map (succ . read) 
              . splitOn "," 
              . head 
              . lines

parseBoards :: String -> [[[Int]]]
parseBoards = map ( map ( map (succ . read) . words ) )
             . splitOn [[]] 
             . drop 2 
             . lines

parse :: String -> ([Int], [[[Int]]])
parse s = (parseNumbers s, parseBoards s)

main = print  . solve . parse =<< readFile . head =<< getArgs 
