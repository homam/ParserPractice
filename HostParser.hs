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
import Data.List (intersperse, isInfixOf)
import System.Environment (getArgs)

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace
 
main :: IO () 
main = do  
    inOrOut <- getArgs >>= return <$> (\x -> if x == "in" then InOffice else OutOffice) . head
    handle <- openFile "host" ReadMode  
    contents <- hGetContents handle  
    writeHostFile $ parseWithEof document contents >>= return <$> foldr1 (\a b -> a ++ "\n" ++ b) . map (toString . transform inOrOut) 
    hClose handle  


writeHostFile :: Either ParseError String -> IO ()
writeHostFile (Left e) = print $ show e
writeHostFile (Right content) = do
    handle <- openFile "./hostw" WriteMode
    hPutStr handle content
    hClose handle  


data Line = Empty | Comment String | Command String String
    deriving Show

data InOrOut = InOffice | OutOffice

transform :: InOrOut -> Line -> Line
transform inOrOut (Command a b) = 
    let [check, replace] = getParams inOrOut in
        if a == check then Command replace b else 
            Command a b
    where
        getParams InOffice = ["80.227.47.62", "192.168.1.42"]
        getParams OutOffice = reverse $ getParams InOffice
transform _ a = a

toString :: Line -> String
toString Empty = ""
toString (Comment a) = '#' : a
toString (Command a b) = a ++ " " ++ b



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
        ip = lexeme (try ip4 <|> ip6)
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
    ips <- ipdigit `sepBy1` string "::"
    return $ foldr1 (\a b -> a ++ "::" ++ b) ips
    where
        ipdigit = many (noneOf "\t: ")