{-# LANGUAGE LambdaCase #-}

import Data.List (nub)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \case
  "part1":_ -> run p1
  "part2":_ -> run p2
  _         -> print "part1 or part2"
  where run f = print . f . parse =<< readFile "./input.txt"

---------------- PART 1 ---------------- 

p1 :: [[Int]] -> Int
p1 = sum . map (solve ((+) . last))

---------------- PART 2 ---------------- 

p2 :: [[Int]] -> Int
p2 = sum . map (solve ((-) . head))

---------------- SHARED ---------------- 

solve :: ([Int] -> (Int -> Int)) -> [Int] -> Int
solve f xs | isUniform = f xs $ head result
           | otherwise = f xs $ solve f result
  where result    = zipWith (-) (tail xs) xs
        isUniform = (length . nub $ result) == 1

---------------- PARSER ---------------- 

parse :: String -> [[Int]]
parse = map (map read . words) . lines
