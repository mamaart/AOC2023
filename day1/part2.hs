import Data.Char (isDigit)

main :: IO()
main = interact $ show . sum . map f . lines

f :: String -> Int
f xs = read $ head x : [last x]
      where x   = run xs

run :: String -> String
run [] = []
run xs = f 0
  where f n 
          | n <= length xs = f' . checkNum . drop n $ xs
          | otherwise      = []
          where f' Nothing  = f (n+1)
                f' (Just v) = v : f (n+1)

checkNum :: String -> Maybe Char
checkNum [] = Nothing
checkNum (x:xs)
  | isDigit x = Just x
  | otherwise = f . length $ xs
  where f 0 = Nothing
        f n = f' . parse . take (n+1) $ (x:xs)
          where f' Nothing  = f (n-1)
                f' (Just v) = Just v


parse :: String -> Maybe Char
parse "one"   = Just '1'
parse "two"   = Just '2'
parse "three" = Just '3'
parse "four"  = Just '4'
parse "five"  = Just '5'
parse "six"   = Just '6'
parse "seven" = Just '7'
parse "eight" = Just '8'
parse "nine"  = Just '9'
parse _       = Nothing

