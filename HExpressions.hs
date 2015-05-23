import Text.Parsec.String (Parser)
import Text.Parsec.String.Char (oneOf, char, digit, letter, satisfy)
import Text.Parsec.String.Combinator (many1, chainl1)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many, (<$))
import Control.Monad (void, ap)
import Data.Char (isLetter, isDigit)
import FunctionsAndTypesForParsing


whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

integer :: Parser Integer
integer = read <$> lexeme (many1 digit)

identifier :: Parser String
identifier = lexeme $ (:) <$> (letter <|> char '_') <*> many1 (digit <|> letter <|> char '_')

symbol :: Char -> Parser ()
symbol c = void $ lexeme $ char c


--iden :: Parser String
--iden =  do
--    firstLetter <- letter <|> char '_'
--    restLetters <- many1 (digit <|> letter <|> char '_')
--    return $ firstLetter : restLetters


data SimpleExpr = Num Integer
    | Var String
    | Add SimpleExpr SimpleExpr
    | Parens SimpleExpr
      deriving (Eq,Show)


var :: Parser SimpleExpr
var = Var <$> identifier

num :: Parser SimpleExpr
num = Num <$> integer

betweenParens :: Parser a -> Parser a
betweenParens p = symbol '(' *> p <* symbol ')'

parens :: Parser SimpleExpr
parens = Parens <$> betweenParens simpleExpr

term :: Parser SimpleExpr
term = num <|> var <|> parens

simpleExpr :: Parser SimpleExpr
simpleExpr = chainl1 term op
  where op = Add <$ lexeme (char '+')