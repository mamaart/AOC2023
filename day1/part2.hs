import Data.Char (isDigit)
import Data.Maybe (mapMaybe)

main :: IO ()
main = interact $ show . work . parse 

work :: [String] -> Int
work = sum . map (\x -> read $ head x : [last x])

parse :: String -> [String]
parse = map (concatMap (mapMaybe conv . rev) . fwd) . lines

conv :: String -> Maybe Char
conv xs 
  | isDigit x     = Just x
  | xs == "one"   = Just '1'
  | xs == "two"   = Just '2'
  | xs == "three" = Just '3'
  | xs == "four"  = Just '4'
  | xs == "five"  = Just '5'
  | xs == "six"   = Just '6'
  | xs == "seven" = Just '7'
  | xs == "eight" = Just '8'
  | xs == "nine"  = Just '9'
  | otherwise     = Nothing
  where x = head xs

rev :: String -> [String]
rev = map reverse . fwd . reverse

fwd :: String -> [String] 
fwd = foldr (\c acc -> case acc of 
  []    -> [[c]]
  (x:_) -> (c:x) : acc ) []
