import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)

main :: IO ()
main = interact part1 

------------------------ PART ONE ----------------------

part1 :: String -> String
part1 input = case parse almanak "" input of 
  Left err -> "error while parsing: " ++ show err
  Right almanak -> "Result: " ++ show (trav almanak)

trav :: Almanak -> Int
trav (Almanak seeds maps) = minimum $ map (`findAll` maps) seeds

findAll :: Int -> [[Map]] -> Int
findAll = foldl find 

-- find the exit num
find :: Int -> [Map] -> Int
find seed []                       = seed
find seed ((Map a b range):rest) 
  | seed >= b && seed <= b + range = seed - b + a -- I think
  | otherwise                      = find seed rest

------------------------ PART TWO ----------------------


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
