import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Data.Either (rights)

main :: IO ()
main = interact $ part2 . myparse

------------------------ PART ONE ----------------------

part1 :: [Card] -> String
part1 = show . sum . map (\(Card _ xs ys) -> f 1 xs ys)
  where f n [] _      = n `div` 2
        f n (x:xs) ys = if   x `elem` ys then f (n+n) xs ys else f n xs ys

------------------------ PART TWO ----------------------

part2 :: [Card] -> String
part2 xs = show $ length xs + f xs
  where f [] = 0
        f ((Card id xs ys): cs) = 1 + f (take (m xs ys) cs) + f cs
        m [] _ = 0
        m (x:xs) ys = if x `elem` ys then 1 + m xs ys else m xs ys

------------------------   DEBUG   ---------------------

check :: [Card] -> String
check = show . map (\(Card id xs ys) -> (length xs, length ys))

------------------------  MODELS  ----------------------

data Card = Card { iden :: Int, winNum :: [Int], myNum :: [Int] } deriving (Show)

------------------------  PARSER  ----------------------

ccard :: Parser Card
ccard = Card <$> key <*> manyTill num (char '|') <* spaces <*> many num

key :: Parser Int
key = string "Card" *> spaces *> num <* char ':' <* spaces

num :: Parser Int
num = read <$> many1 digit <* spaces

myparse :: String -> [Card]
myparse = rights . map (parse ccard "") . lines


------------------------   EOF   -----------------------
