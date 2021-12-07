import Data.List
import System.Environment 
import Data.List.Split (splitOn)

-- solving

board_score :: [[Int]] -> Int
board_score = sum . map pred . filter (>0) . concat

check_board :: [[Int]] -> Bool
check_board b = not . null . filter (all (<0)) $ (b++bT) 
            where bT = transpose b

check :: [[[Int]]] -> [Int]
check = map fst . filter (check_board . snd) . zip [0..]

play :: Int -> [[[Int]]] -> [[[Int]]] 
play n = map $ map $ map (\x -> if x == n then -x else x)

solve :: ([Int], [[[Int]]]) -> Int
solve ((h:t),[b])
     | check_board b' = (h-1) * board_score b' 
     | otherwise = solve (t,[b'])
    where [b'] = play h [b]
solve ((h:t), bs) = case check bs' of 
     [] -> solve (t, bs')
     l  -> solve (t, [b | (i,b) <- zip [0..] bs', not $ i `elem` l] )
    where bs' = play h bs 


-- parsing

-- increased the number by 1.
-- Now, a marked number is a negative number
-- Note: before scoring subtract 1 to all the number in the board.

parse_numbers :: String -> [Int]
parse_numbers = map (succ . read) 
              . splitOn "," 
              . head 
              . lines

parse_boards :: String -> [[[Int]]]
parse_boards = map (map (map (succ . read) . words)) 
             . splitOn [[]] 
             . drop 2 
             . lines

parse :: String -> ([Int], [[[Int]]])
parse s = (parse_numbers s, parse_boards s)

main = print  . solve . parse =<< readFile =<< head <$> getArgs 
