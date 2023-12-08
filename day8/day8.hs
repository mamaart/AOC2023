import Text.Parsec (char, letter, newline, between, parse, many1, (<|>), string, count, spaces)
import Text.Parsec.String (Parser)
import Data.Functor (($>))

main :: IO ()
main = print . fmap part1 . parse myMap "" =<< readFile "./myinput.txt"

---------------- TRANNY ----------------

part1 :: Input -> Maybe Int
part1 (Input directions theMap) = go directions "AAA" 0
  where go _ "ZZZ" i      = Just i ----------------------- Final destination
        go [] label i     = go directions label i -------- Reset the directions
        go (d:ds) label i = case lookup label theMap of -- Find left and right in map
           Nothing -> Nothing ---------------------------- The label does not exist
           Just(l, r) | l == r && label == l -> Nothing -- We have hit a wrong leaf 
                     | otherwise -> case d of ------------ Check direction
                       L -> go ds l (i+1) ---------------- Go left increment with 1
                       R -> go ds r (i+1) ---------------- Go right increment with 1

---------------- PARSER ----------------

data Direction = L | R deriving (Show)

data Input = Input [Direction] [(String, (String, String))] deriving (Show)

---------------- PARSER ----------------

myMap :: Parser Input
myMap = Input <$> many1 direction <* many1 newline <*> many1 node 

direction :: Parser Direction
direction = (char 'L' $> L) <|> (R <$ char 'R')

node :: Parser (String, (String, String))
node = (,) <$> key <*> between (char '(') (char ')') neighbors <* newline

key :: Parser String
key = name <* string "=" <* spaces

neighbors :: Parser (String, String)
neighbors = (,) <$> name <* char ',' <* spaces <*> name

name :: Parser [Char]
name = count 3 letter <* spaces
