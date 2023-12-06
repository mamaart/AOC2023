import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = interact $ show . fmap part2 . parse laps ""

------------------ PART 1 ------------------

part1 :: Laps -> Int
part1 (Laps xs ys) = foldr (\(a, b) acc -> find a b * acc) 1 $ zip xs ys

------------------ PART 2 ------------------

part2 :: Laps -> Int
part2 (Laps xs ys) = find (f xs) (f ys)
  where f = read . foldr (\x acc -> show x ++ acc) "" 

------------------ SHARED ------------------

find :: Int -> Int -> Int
find a y = upper (d (+)) - lower (d (-)) + 1
  where d f     = (fromIntegral a `f` sqrt  (fromIntegral ((a ^ 2) - (4 * y)))) / 2
        upper x = if x == fromIntegral (round x) then round x - 1 else floor x
        lower x = if x == fromIntegral (round x) then round x + 1 else ceiling x

------------------ MODELS ------------------

data Laps = Laps [Int] [Int] deriving (Show)

------------------ PARSER ------------------

laps :: Parser Laps
laps = Laps 
  <$> (string "Time:" *> spaces *> nums) 
  <*> (string "Distance:" *> spaces *> nums)

nums :: Parser [Int]
nums = manyTill (read <$> many1 digit <* optional (many (char ' '))) (char '\n')
