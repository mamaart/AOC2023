import Data.Char (isDigit)

main = interact $ show . sum . map f . lines

f :: String -> Int
f s = read $ head ds : [last ds]
      where ds = filter isDigit s

