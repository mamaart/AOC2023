{-# LANGUAGE LambdaCase #-}
import Text.Parsec 
import Text.Parsec.String (Parser)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
    
main :: IO ()
main = getArgs >>= \case 
  ["part1"] -> run part1
  ["part2"] -> run part2
  _ -> print "part1 or part2?"
  where run f = interact $ (++ "\n") . show . f . ppparse

------------------ PART 1 ------------------

part1 :: [Game] -> Int
part1 = sum . map ((\(RGB iden (r, g, b)) -> 
  if r <= 12 && g <= 13 && b <= 14 
  then iden 
  else 0) . toRGB)

------------------ PART 2 ------------------

part2 :: [Game] -> Int
part2 = sum . map ((\(RGB _ (r,g,b)) -> r*g*b) . toRGB)

------------------ SHARED ------------------

toRGB :: Game -> RGB
toRGB (Game iden colors) = RGB { iden = iden , vals = (r, g, b)}
  where r = max (\case { R x -> x; _ -> 0}) 
        g = max (\case { G x -> x; _ -> 0}) 
        b = max (\case { B x -> x; _ -> 0}) 
        max f = maximum $ map f colors 

------------------ MODELS ------------------

data Game = Game Int [Color] deriving (Show)
data Color = R Int | G Int | B Int deriving (Show, Eq)
data RGB = RGB{ iden :: Int, vals :: (Int, Int, Int)} deriving (Show, Eq)

------------------ PARSER ------------------

ppparse :: String -> [Game]
ppparse = mapMaybe (pparse game) . lines

pparse :: Parser a -> String -> Maybe a
pparse f = either (const Nothing) Just . parse f ""

game :: Parser Game
game = Game <$> key <*> colors

colors :: Parser [Color]
colors = sepBy color (char ',' <* space <|> char ';' <* space)

key :: Parser Int
key = string "Game" <* space *> number <* char ':' <* space

color :: Parser Color
color = try blue <|> try green <|> red 

red   = R <$> number <* space <* string "red"
green = G <$> number <* space <* string "green"
blue  = B <$> number <* space <* string "blue"

number :: Parser Int
number = read <$> many1 digit
