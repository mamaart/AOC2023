import Text.Parsec
import Text.Parsec.String
import Data.List (group, nub, sort)
import Data.Functor 

main :: IO ()
main = interact $ either show ((++"\n") . show . part2) . mapM (fmap rawToHand .  parse rawHand "") . lines

------------------ RART 2 ------------------ 

part2 :: [Hand] -> Int
part2 hs = fst . foldr (\(Hand _ _ bid) (acc, i) -> ( bid * i + acc,i-1 )) (0, length hs) $ sort hs

rawToHand ::RawHand -> Hand 
rawToHand (RawHand cards bid) = Hand (getBestType cards) cards bid

getBestType :: Cards -> Type
getBestType cards = case result of 
  [] -> KindFive
  x  -> maximum result
  where result = combinations cards

combinations :: Cards -> [Type]
combinations (Cards a b c d e) 
  = [getType $ Cards a' b' c' d' e' 
      | a' <- replace a,
        b' <- replace b,
        c' <- replace c,
        d' <- replace d,
        e' <- replace e]
  where replace x 
          | x == Joker = filter (/= Joker) [a, b, c, d, e] 
          | otherwise = [x]

------------------  SHARED ------------------ 

getType :: Cards -> Type
getType (Cards a b c d e) 
  | 5 `kind` g               = KindFive
  | 4 `kind` g               = KindFour
  | 3 `kind` g && 1 `pair` g = House
  | 3 `kind` g               = KindThree
  | 2 `pair` g               = TwoPair
  | 1 `pair` g               = Pair
  | otherwise                = High
  where g      = (group . sort) [a, b, c, d, e]
        pair n = ((==n) . length) . filter ((==2) . length)
        kind n = any $ (==n) . length

------------------  SORT  ------------------ 

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

data Type = High| Pair | TwoPair | KindThree | House | KindFour | KindFive  deriving (Enum, Show, Eq, Ord)

data Rank = Joker | Two | Three | Four | Five | Six | Seven | Eight| Nine | Ten | Queen | King | Ace deriving (Enum, Show, Eq, Ord)

------------------ PARSER ------------------ 

rawHand :: Parser RawHand
rawHand = RawHand 
  <$> (Cards <$> rank <*> rank  <*> rank  <*> rank  <*> rank <* spaces) 
  <*> (read <$> many1 digit)
  where rank = choice [ 
          char '2' $> Two,  char '3' $> Three, char '4' $> Four, char '5' $> Five
         ,char '6' $> Six,  char '7' $> Seven, char '8' $> Eight,char '9' $> Nine
         ,char 'T' $> Ten,  char 'J' $> Joker, char 'Q' $> Queen
         ,char 'K' $> King, char 'A' $> Ace,   fail "ivalid rank"]
