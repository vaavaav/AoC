import Data.List
import System.Environment 
import Data.List.Split (splitOn)

-- solving

boardScore :: [[Int]] -> Int
boardScore = sum . map pred . filter (>0) . concat

checkBoard :: [[Int]] -> Bool
checkBoard b = any (all (< 0)) (b++bT) 
  where bT = transpose b

check :: [[[Int]]] -> [Int]
check = map fst . filter (checkBoard . snd) . zip [0..]

play :: Int -> [[[Int]]] -> [[[Int]]] 
play n = map $ map $ map (\x -> if x == n then -x else x)

solve :: ([Int], [[[Int]]]) -> Int
solve ( h:t ,[b])
     | checkBoard b' = (h-1) * boardScore b' 
     | otherwise = solve (t,[b'])
    where [b'] = play h [b]
solve ( h:t , bs) = case check bs' of 
     [] -> solve (t, bs')
     l  -> solve (t, [b | (i,b) <- zip [0..] bs', i `notElem` l] )
    where bs' = play h bs 


-- parsing

-- increased the number by 1.
-- Now, a marked number is a negative number
-- Note: before scoring subtract 1 to all the number in the board.

parseNumbers :: String -> [Int]
parseNumbers = map (succ . read) 
              . splitOn "," 
              . head 
              . lines

parseBoards :: String -> [[[Int]]]
parseBoards = map (map (map (succ . read) . words)) 
             . splitOn [[]] 
             . drop 2 
             . lines

parse :: String -> ([Int], [[[Int]]])
parse s = (parseNumbers s, parseBoards s)

main = print  . solve . parse =<< readFile . head =<< getArgs 
