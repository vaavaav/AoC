import Data.List as L
import System.Environment 
import Data.HashMap.Strict as HM (HashMap, fromList, (!), member, map, adjust, empty, insertWith)
import Data.Heap as H

type Point = (Int,Int)
type HeapStar = Heap FstMinPolicy (Float,Point)
type Grid  = HashMap Point Int
type Path  = HashMap Point Point
type Score = HashMap Point Float
type Heuristic = (Point -> Float)

reconstructPath :: Point -> Path -> [Point]
reconstructPath curr p 
  | curr `member` p = curr : reconstructPath (p ! curr) p
  | otherwise = []

tentative_gScore :: Heuristic -> Point -> Point -> (HeapStar, Path, Score) -> (HeapStar, Path, Score)
tentative_gScore h curr neighbor (openSet, cameFrom, score)
    | t_gScore < (score ! neighbor) = (openSet', cameFrom', score') 
    | otherwise = (openSet, cameFrom, score) 
 where t_gScore = (score ! curr) + (h neighbor)
       openSet' = if neighbor `elem` openSet then openSet else H.insert (score' ! neighbor, neighbor) openSet
       cameFrom' = insertWith const neighbor curr cameFrom
       score' = adjust (const t_gScore) neighbor score

run :: Heuristic -> Point -> HeapStar -> Path -> Score -> Path
run h goal openSet cameFrom score
     | curr == goal = cameFrom
     | otherwise = run h goal openSet'' cameFrom' score'
 where ([(_,curr@(x,y))],openSet') = H.splitAt 1 openSet
       neighbors = L.filter (`member` score) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
       (openSet'', cameFrom', score') = foldr (tentative_gScore h curr) (openSet', cameFrom, score) neighbors

-- https://en.wikipedia.org/wiki/A*_search_algorithm
aStar :: Heuristic -> Point -> Point -> Grid -> Path
aStar h start goal grid = run h goal openSet cameFrom score
    where openSet = H.singleton (score ! start, start)
          cameFrom = HM.empty 
          score = adjust (const (h start)) start $ HM.map (const (1/0)) grid

solve m =  sum
        $ L.map (grid !)
        $ reconstructPath goal
        $ aStar heuristic start goal grid
 where (ys,xs) = (length m, length $ head m)
       heuristic = fromIntegral . (grid !)
       grid = HM.fromList $ zip [(x,y) | y <- [0..ys-1], x <- [0..xs-1]] $ concat m
       goal = (xs-1,ys-1)
       start = (0,0)
       
      

parse :: String -> [[Int]]
parse = L.map (L.map (read . pure)). lines

main = print . solve . parse =<< readFile . head =<< getArgs
