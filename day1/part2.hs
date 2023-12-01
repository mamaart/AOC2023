import Data.Char (isDigit)
import Data.Maybe (mapMaybe)

main :: IO ()
main = interact $ show . work . parse 

work :: [String] -> Int
work = sum . map (\x -> read $ head x : [last x])

parse :: String -> [String]
parse = map (concatMap nums . fwd) . lines

nums :: String -> String
nums [] = []
nums (x:xs) 
  | isDigit x = [x] 
  | otherwise = mapMaybe conv . rev $ (x:xs)

conv :: String -> Maybe Char
conv "one"   = Just '1'
conv "two"   = Just '2'
conv "three" = Just '3'
conv "four"  = Just '4'
conv "five"  = Just '5'
conv "six"   = Just '6'
conv "seven" = Just '7'
conv "eight" = Just '8'
conv "nine"  = Just '9'
conv _       = Nothing

rev :: String -> [String]
rev = map reverse . fwd . reverse

fwd :: String -> [String] 
fwd = foldr (\c acc -> case acc of 
  []    -> [[c]]
  (x:_) -> (c:x) : acc ) []
