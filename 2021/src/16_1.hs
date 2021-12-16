import Data.List
import System.Environment 

fromBinary :: [Int] -> Int
fromBinary = foldl ((+) . (2*)) 0

fromHex :: Char -> [Int]
fromHex '0' = [0,0,0,0]
fromHex '1' = [0,0,0,1]
fromHex '2' = [0,0,1,0]
fromHex '3' = [0,0,1,1]
fromHex '4' = [0,1,0,0]
fromHex '5' = [0,1,0,1]
fromHex '6' = [0,1,1,0]
fromHex '7' = [0,1,1,1]
fromHex '8' = [1,0,0,0]
fromHex '9' = [1,0,0,1]
fromHex 'A' = [1,0,1,0]
fromHex 'B' = [1,0,1,1]
fromHex 'C' = [1,1,0,0]
fromHex 'D' = [1,1,0,1]
fromHex 'E' = [1,1,1,0]
fromHex 'F' = [1,1,1,1]

getLiteral :: [Int] -> [Int]
getLiteral (0:t) = take 4 t
getLiteral (1:t) = take 4 t ++ getLiteral (drop 4 t) 


decode :: [Int] -> [[Int]]
decode p@(v1:v2:v3:1:0:0:r) = [[p]]
decode p@(v1:v2:v3:_:_:_:i:r)
   | i == 0 = (take (22 + len0) p) : decodeSub0 len0 (drop 22 p) 
   | otherwise = (take (18 + len1') p) : r
  where len0 = fromBinary $ take 15 r
        len1 = fromBinary $ take 11 r
        (len1', r) = decodeSub1 len1 (drop 18 p) 


solve = decode

parse :: String -> [Int]
parse = concatMap fromHex

main = print . solve . parse =<< readFile . head =<< getArgs
