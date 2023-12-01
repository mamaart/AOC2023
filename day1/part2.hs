import Data.Char (isDigit)
import Data.Maybe (mapMaybe)

main :: IO ()
main = interact $ show . work . parse 

work :: [String] -> Int
work = sum . map (\x -> read $ head x : [last x])

parse :: String -> [String]
parse = map (concatMap (mapMaybe conv . rev) . fwd) . lines

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
