import System.IO  
import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.String.Parsec (try)
import Text.Parsec.String.Char
import Text.Parsec.String.Combinator 
import Control.Applicative ((<$>), (<*>), (<$), (<*), (*>), (<|>), many)
import Control.Monad (void, ap, mzero)
import Data.Char (isLetter, isDigit)
import FunctionsAndTypesForParsing
import Data.List (intersperse)


whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace
 
main :: IO () 
main = do  
    handle <- openFile "host" ReadMode  
    contents <- hGetContents handle  
    --putStrLn $ show $ regularParse line contents -- (try (many line) <|> eof') contents --contents  
    print $ parseWithEof document contents
    hClose handle  


data Line = Empty | Comment String | Command String String
    deriving Show

document :: Parser [Line]
document = line `endBy` char '\n'

line :: Parser Line
line = (eof >> return Empty) <|> comment <|> command

comment :: Parser Line
comment = Comment <$> do
    void $ char '#'
    many (noneOf "\n")
    --anyChar `manyTill` char '\n'

command :: Parser Line
command = Command <$> ip <*> host
    where 
        ip = lexeme ip4
        host = many (noneOf "\n")

ip4 :: Parser String
ip4 = do
    ips <- ipdigit `sepBy1` char '.'
    return $ foldr1 (\a b -> a ++ "." ++ b) ips
    where 
        ipdigit = do 
            i <- many1 digit
            let j = read i :: Integer in
                if j >= 0 && j < 256 then return i else fail "Expected a number less than 256"

ip6 :: Parser String
ip6 = do
    ips <- ipdigit `sepBy1` char ':'
    return $ foldr1 (\a b -> a ++ ":" ++ b) ips
    where
        ipdigit = many (noneOf "\t ")