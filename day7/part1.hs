import Text.Parsec
import Text.Parsec.String
import Data.List (group, nub, sort)

main :: IO ()
main = interact $ either show ((++"\n") . show . part1) . mapM (fmap rawToHand .  parse rawHand "") . lines

------------------ RART 1 ------------------ 

part1 :: [Hand] -> Int
part1 hs = fst . foldr (\(Hand _ _ bid) (acc, i) -> (bid*i + acc,i-1)) (0, length hs) $ sort hs

rawToHand ::RawHand -> Hand 
rawToHand (RawHand cards bid) = Hand (getType cards) cards bid

------------------ SHARED ------------------ 

getType :: Cards -> Type
getType (Cards a b c d e) 
  | 5 `kind` g               = KindFive
  | 4 `kind` g               = KindFour
  | 3 `kind` g && 1 `pair` g = House
  | 3 `kind` g               = KindThree
  | 2 `pair` g               = TwoPair
  | 1 `pair` g               = Pair
  | otherwise                = High
  where g = (group . sort) [a, b, c, d, e]

pair :: Int -> [[Rank]] -> Bool
pair n = ((==n) . length) . filter ((==2) . length)

kind :: Int -> [[Rank]] -> Bool
kind n = any $ (== n) . length

instance Ord Hand where 
  ( Hand t1 h1 _ ) `compare` ( Hand t2 h2 _ ) 
    = if t1 == t2 then h1 `compare` h2 else t1 `compare` t2

instance Ord Cards where 
  (Cards a1 b1 c1 d1 e1) `compare` (Cards a2 b2 c2 d2 e2 ) 
    | a1 /= a2  = a1 `compare` a2
    | b1 /= b2  = b1 `compare` b2
    | c1 /= c2  = c1 `compare` c2
    | d1 /= d2  = d1 `compare` d2
    | otherwise = e1 `compare` e2

------------------ MODELS ------------------ 

data Hand = Hand Type Cards Int deriving (Show, Eq)

data Cards = Cards Rank Rank Rank Rank Rank deriving (Show, Eq)

data RawHand = RawHand Cards Int deriving (Show)

data Type = High
          | Pair 
          | TwoPair 
          | KindThree 
          | House 
          | KindFour
          | KindFive  deriving (Enum, Show, Eq, Ord)

data Rank = Undefined 
          | Two 
          | Three 
          | Four 
          | Five 
          | Six 
          | Seven 
          | Eight 
          | Nine 
          | Ten 
          | Jack
          | Queen 
          | King 
          | Ace deriving (Enum, Show, Eq, Ord)

------------------ PARSER ------------------ 

conv :: Char -> Rank
conv '2' = Two
conv '3' = Three
conv '4' = Four
conv '5' = Five
conv '6' = Six
conv '7' = Seven
conv '8' = Eight
conv '9' = Nine
conv 'T' = Ten
conv 'J' = Jack
conv 'Q' = Queen
conv 'K' = King
conv 'A' = Ace
conv  _  = Undefined

rawHand :: Parser RawHand
rawHand = RawHand <$> cards <*> (read <$> many1 digit)

cards :: Parser Cards
cards = Cards <$> rank <*> rank  <*> rank  <*> rank  <*> rank <* spaces

rank :: Parser Rank 
rank = conv <$> anyChar 
