import Data.Char (isDigit)
import Data.Maybe (mapMaybe)

main :: IO()
main = interact $ show . work . parse 

work :: [String] -> Int
work = sum . map (\x -> read $ head x : [last x])

parse :: String -> [String]
parse = map (concatMap getNum . trav) . lines

getNum :: String -> String
getNum [] = []
getNum (x:xs) = if isDigit x then [x] else mapMaybe convert . revtrav $ (x:xs)

convert :: String -> Maybe Char
convert "one"   = Just '1'
convert "two"   = Just '2'
convert "three" = Just '3'
convert "four"  = Just '4'
convert "five"  = Just '5'
convert "six"   = Just '6'
convert "seven" = Just '7'
convert "eight" = Just '8'
convert "nine"  = Just '9'
convert _       = Nothing

revtrav :: [Char] -> [[Char]]
revtrav = map reverse . trav . reverse

trav :: [Char] -> [[Char]]
trav = foldr (\c acc -> case acc of [] -> [[c]]; (x:xs) -> (c : x) : acc) []
