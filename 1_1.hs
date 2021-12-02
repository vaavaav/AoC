import Data.List
import System.Environment

main = print
     . length
     . filter (uncurry (<))
     . (\l -> zip l (tail l))
     . map (read :: String -> Int)
     . lines
     =<< readFile 
     =<< head <$> getArgs 
