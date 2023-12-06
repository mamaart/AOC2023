{-# LANGUAGE LambdaCase #-}
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \case
  ["part1"] -> run $ part1 . map (filter isDigit) 
  ["part2"] -> run $ part2 . map (concatMap (mapMaybe conv . rev) . fwd) 
  _         -> print "part1 or part2?"
  where run f = interact $ (++ "\n") . show . f . lines

------------------ PART 1 ------------------

part1 :: [String] -> Int
part1 = sum . map (\xs -> read $ head xs: [last xs])

------------------ PART 2 ------------------

part2 :: [String] -> Int
part2 = sum . map (\x -> read $ head x : [last x])

conv :: String -> Maybe Char
conv xs = case xs of 
  [x] | isDigit x -> Just x
  "one"           -> Just '1'
  "two"           -> Just '2'
  "three"         -> Just '3'
  "four"          -> Just '4'
  "five"          -> Just '5'
  "six"           -> Just '6'
  "seven"         -> Just '7'
  "eight"         -> Just '8'
  "nine"          -> Just '9'
  _               -> Nothing

rev :: String -> [String]
rev = map reverse . fwd . reverse

fwd :: String -> [String] 
fwd = foldr (\c acc -> case acc of 
  []    -> [[c]]
  (x:_) -> (c:x) : acc ) []
