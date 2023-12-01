import Data.Char (isDigit)

main :: IO ()
main = interact $ show . work . parse

parse :: String -> [String]
parse = map (filter isDigit) . lines

work :: [String] -> Int
work = sum . map (\xs -> read $ head xs: [last xs])
