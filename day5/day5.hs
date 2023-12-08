{-# LANGUAGE LambdaCase #-}

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Data.List (foldl', insert)
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

part2 :: Almanak -> Either String Int
part2 (Almanak s m) = minimum . (\r -> map fst $ f r m) <$> rng s

f :: [(Int, Int)] -> [[Map]] -> [(Int, Int)]
f ps []       = ps
f ps (ms:mss) = foldr (\p acc -> f ( transform p ms ) mss ++ acc  ) [] ps

transform :: (Int, Int) -> [Map] -> [(Int, Int)]
transform p [] = [p]
transform (inputStart, inputLen) ((Map dstStart srcStart mapLen) : ms)
  | isStartInRange  && isEndInRange  = [(convert inputStart, convert inputLen)]
  | isStartOutRange && isEndOutRange = transform (inputStart, inputLen) ms
  | isStartOutRange && isEndInRange  = transform (inputStart, srcStart-1) ms ++ [(convert srcStart, convert inputEnd)]
  | isStartInRange  && isEndOutRange = (convert inputStart, convert srcEnd)   : transform (srcEnd, inputEnd) ms
  | otherwise = [(-1000, -1000)]
  where inputEnd        = inputStart + inputLen
        srcEnd          = srcStart + mapLen
        convert x       = x - srcStart + dstStart
        isStartInRange  = inputStart >= srcStart && inputStart <= srcEnd
        isEndInRange    = inputEnd <= srcEnd && inputEnd >= srcStart
        isStartOutRange = inputStart < srcStart || inputStart > srcEnd
        isEndOutRange   = inputEnd > srcEnd || inputEnd < srcStart

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
