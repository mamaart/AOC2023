{-# LANGUAGE LambdaCase #-}

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Data.List (foldl')
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \case 
    "part1":_ -> run part1
    "part2":_ -> run part2
    _ -> print "part1 or part2?"
    where run f = interact $ (++ "\n") . show . fmap f . parse almanak "" 

------------------------ PART ONE ----------------------

part1 :: Almanak -> Int
part1 (Almanak seeds maps) = minimum $ map (\s -> foldl' find s maps) seeds

find :: Int -> [Map] -> Int
find seed []                       = seed
find seed ((Map dst src rng):rest) 
  | seed >= src && seed <= src + rng = seed - src + dst -- I think
  | otherwise                        = find seed rest

------------------------ PART TWO ----------------------

rng :: [Int] -> Either String [(Int, Int)]
rng []  = Right []
rng [_] = Left "one element list is not a range"
rng (a:b:xs) = (:) <$> Right (a,b) <*> rng xs

part2 :: Almanak -> Either String [Int]
part2 (Almanak s m) = (\r -> map fst $ f r m) <$> rng s

f :: [(Int, Int)] -> [[Map]] -> [(Int, Int)]
f ps []       = ps
f ps (ms:mss) = foldr ( \p acc -> transform p ms ++ acc ) [] ps

transform :: (Int, Int) -> [Map] -> [(Int, Int)]
transform p [] = [p]
transform (start, len) ((Map dst src rng) : ms)
  | start >= src && start + len <= src + rng = [(start-src+dst, (start+len)-src+dst)]
  | start >= src && start + len >= src + rng = (start-src+dst, start-src+dst+rng) : transform (start+rng, len-rng) ms
  | start <= src && start + len <= src + rng = transform (start, len-src) ms ++ [(src+dst, dst+rng-src-len)]          -- this one might be wrong
  | otherwise = transform (start, len) ms

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
