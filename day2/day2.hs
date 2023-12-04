    {-# LANGUAGE LambdaCase #-}
    import Text.Parsec 
    import Text.Parsec.String (Parser)
    import Data.Maybe (mapMaybe)
        
    main :: IO ()
    main = interact $ show . part2 . ppparse
    
    data Game = Game Int [Color] deriving (Show)
    data Color = R Int | G Int | B Int deriving (Show, Eq)
    data RGB = RGB{ iden :: Int, vals :: (Int, Int, Int)} deriving (Show, Eq)
    
    part1 :: [Game] -> Int
    part1 = sum . map ((\(RGB iden (r, g, b)) -> if r <= 12 && g <= 13 && b <= 14 then iden else 0) . toRGB)
    
    part2 :: [Game] -> Int
    part2 = sum . map ((\(RGB _ (r,g,b)) -> r*g*b) . toRGB)
    
    toRGB :: Game -> RGB
    toRGB (Game iden colors) = RGB { iden = iden , vals = (r, g, b)}
      where r = max (\case { R x -> x; _ -> 0}) 
            g = max (\case { G x -> x; _ -> 0}) 
            b = max (\case { B x -> x; _ -> 0}) 
            max f = maximum $ map f colors 
    
    -- PARSING
    
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
