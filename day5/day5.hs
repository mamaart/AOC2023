import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Data.List (foldl')

main :: IO ()
main = interact $ show . fmap part2 . parse almanak "" 

------------------------ PART ONE ----------------------

part1 :: Almanak -> Int
part1 (Almanak seeds maps) = minimum $ map (\s -> foldl' find s maps) seeds

------------------------ PART TWO ----------------------

rng :: [Int] -> Either String [(Int, Int)]
rng []  = Right []
rng [_] = Left "one element list is not a range"
rng (a:b:xs) = (:) <$> Right (a,b) <*> rng xs

part2 :: Almanak -> Either String Int
part2 (Almanak s m) = foldl' (\acc rng -> min acc (minimum (f rng m)) ) maxBound `fmap` rng s

f ::  (Int, Int)-> [[Map]] -> [Int]
f (_, 0) _    = []
f (a, b) maps = go a b 
  where go acc 0 = [] 
        go acc n = foldl' find acc maps : go (acc+1) (n-1)

------------------------  SHARED  ----------------------

fx :: [[Map]] -> (Int -> Int)
fx (xs:xss) = \x -> 0

find :: Int -> [Map] -> Int
find seed []                       = seed
find seed ((Map a b range):rest) 
  | seed >= b && seed <= b + range = seed - b + a -- I think
  | otherwise                      = find seed rest

------------------------  MODELS  ----------------------

data Almanak = Almanak [Int] [[Map]] deriving (Show)

data Map = Map Int Int Int deriving (Show)

------------------------  PARSER  ----------------------

almanak :: Parser Almanak
almanak = Almanak <$> seedList <* heading <*>  listMaps

seedList :: Parser [Int]
seedList = string "seeds" *> char ':' *> space *> many num <* spaces

mapList :: Parser [Map]
mapList = sepBy parseMap spaces <* spaces

heading :: Parser ()
heading = void $ manyTill anyChar space <* string "map:" <* spaces

listMaps :: Parser [[Map]]
listMaps = sepBy mapList heading <* spaces

num :: Parser Int
num = read <$> many1 digit <* space

parseMap :: Parser Map
parseMap = Map <$> num <*> num <*> num <* spaces

------------------------   EOF   -----------------------
