import Data.Char (isDigit)

main = interact $ show . sum . map f . lines

f :: String -> Int
f xs = read $ fst ++ lst
      where x   = run xs
            fst = show . head $ x
            lst = show . last $ x

run :: String -> [Int]
run [] = []
run xs = f 0
  where f n 
          | n <= length xs = f' . checkNum . drop n $ xs
          | otherwise      = []
          where f' Nothing  = f (n+1)
                f' (Just v) = v : f (n+1)

checkNum :: String -> Maybe Int
checkNum [] = Nothing
checkNum (x:xs)
  | isDigit x = Just . read $ [x]
  | otherwise = f . length $ xs
  where f 0 = Nothing
        f n = f' . parse . take (n+1) $ (x:xs)
          where f' Nothing  = f (n-1)
                f' (Just v) = Just v

parse :: String -> Maybe Int
parse s 
  | s == "one"    = Just 1
  | s == "two"    = Just 2
  | s == "three"  = Just 3
  | s == "four"   = Just 4
  | s == "five"   = Just 5
  | s == "six"    = Just 6
  | s == "seven"  = Just 7
  | s == "eight"  = Just 8
  | s == "nine"   = Just 9
  | otherwise     = Nothing

