import Data.List
import System.Environment


main = print
     . length
     . filter (uncurry (<))
     . (\l -> zip l (tail l))
     . (\l -> zipWith3 (\a b c -> a+b+c) l (tail l) (tail $ tail l))
     . map (read :: String -> Int)
     . lines
     =<< readFile 
     =<< head <$> getArgs 
