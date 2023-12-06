import Text.Parsec.String (Parser)
import Text.Parsec
import Data.Char (isDigit)
import Control.Monad (void)

main :: IO ()
main = print $ pparse test "467..114.....*........35..633.......#...617*...........+.58...592...........755....$.*.....664.598.."

test = many $ anyChar <* skipMany1 (char '.')

pparse :: Parser a -> String -> Maybe a
pparse f = either (const Nothing) Just . parse f ""


(!!!) :: (Ord t, Num t) => [a] -> t -> Maybe a
xs     !!! n | n < 0 =  Nothing
[]     !!! _         =  Nothing
(x:_)  !!! 0         =  Just x
(_:xs) !!! n         =  xs !!! (n-1)
