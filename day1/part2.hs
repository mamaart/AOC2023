import Data.Char (isDigit)

main :: IO()
main = interact $ show . work . parse

work :: [String] -> Int
work = sum . map (\x -> read $ head x : [last x])

parse :: String -> [String]
parse = map getNumbers . lines

getNumbers :: String -> String
getNumbers [] = []
getNumbers xs = f 0
  where f n 
          | n <= length xs = f' . getNumber . drop n $ xs
          | otherwise      = []
          where f' Nothing  = f (n+1)
                f' (Just v) = v : f (n+1)

getNumber :: String -> Maybe Char
getNumber [] = Nothing
getNumber (x:xs)
  | isDigit x = Just x
  | otherwise = f . length $ xs
  where f 0 = Nothing
        f n = f' . convert . take (n+1) $ (x:xs)
          where f' Nothing  = f (n-1)
                f' (Just v) = Just v


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

