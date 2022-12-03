import System.Environment

data Shape = Rock | Paper | Scissors deriving (Enum, Eq)
data Outcome = Lose | Draw | Win deriving Enum

toShape :: Char -> Shape
toShape 'A' = Rock
toShape 'X' = Rock
toShape 'B' = Paper
toShape 'Y' = Paper
toShape 'C' = Scissors
toShape 'Z' = Scissors

shapeValue :: Shape -> Int
shapeValue = succ . fromEnum

toOutcome :: Char -> Outcome
toOutcome 'X' = Lose 
toOutcome 'Y' = Draw
toOutcome 'Z' = Win

outcomeValue :: Outcome -> Int
outcomeValue = (3*) . fromEnum

findShape :: Shape -> Outcome -> Shape
findShape s o = toEnum ((fromEnum s + (fromEnum o) - 1) `mod` 3)

findShape' = uncurry findShape

findOutcome :: Shape -> Shape -> Outcome
findOutcome Rock Paper = Win
findOutcome Paper Scissors = Win
findOutcome Scissors Rock = Win
findOutcome o m 
  | o == m = Draw
  | otherwise = Lose

findOutcome' = uncurry findOutcome

playValue :: (Shape, Outcome) -> Int
playValue = uncurry (+) . (shapeValue >< outcomeValue)

-- Part 1

solve1 = sum . map (playValue . (snd `split` findOutcome'))

parse1 :: String -> [(Shape, Shape)]
parse1 = map f . lines  
      where f [o,' ',m] = (toShape o, toShape m)

-- Part 2

solve2 = sum . map (playValue . (findShape' `split` snd))

parse2 :: String -> [(Shape, Outcome)]
parse2 = map f . lines  
      where f [o,' ',m] = (toShape o, toOutcome m)

-- Parse

main = print . ((solve1 . parse1) `split` (solve2 . parse2)) =<< readFile . head =<< getArgs

---

(f `split` g) x = (f x, g x)
(f >< g) (x,y) = (f x, g y)



