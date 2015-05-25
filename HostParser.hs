{-# LANGUAGE BangPatterns #-}

import System.IO  
import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.String.Parsec (try)
import Text.Parsec.String.Char
import Text.Parsec.String.Combinator 
import Control.Applicative --((<$>), (<*>), (<$), (<*), (*>), (<|>), many)
import Control.Monad --(void, ap, mzero)
-- import Data.Char (isLetter, isDigit)
import FunctionsAndTypesForParsing
import Data.List (intercalate) --, isInfixOf)
import System.Environment (getArgs)


whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace
 
main :: IO () 
main = getArgs >>= execArgs . parseArgs
    where
        execArgs (Left e) = print e
        execArgs (Right (Args inOrOut fileName)) = execute fileName inOrOut

data InOrOut = InOffice | OutOffice deriving Show
data Args = Args InOrOut String

instance Show Args where
    show (Args inOurOut fileName) = show inOurOut ++ " " ++ fileName

parseArgs :: [String] -> Either String Args
parseArgs [inOurOut, fileName] = combine inOurOut'
    where 
        inOurOut'
          | "in" == inOurOut = Right InOffice
          | "out" == inOurOut = Right OutOffice
          | otherwise = Left "Expected in or out"
        combine (Left e) = Left e
        combine (Right inOrOut'') = Right $ Args inOrOut'' fileName

parseArgs _  = Left "Invalid arguments"

execute :: String -> InOrOut -> IO ()
execute fileName inOrOut = do
    contents <- readTextFile fileName
    writeHostFile fileName $ parseWithEof document contents >>= return <$> intercalate "\n" . map (show . transform inOrOut) 

readTextFile :: String -> IO String
readTextFile fileName = do
    handle <- openFile fileName ReadMode  
    contents <- hGetContents handle >>= \x -> return $! x
    hClose handle
    return contents

writeTextFile :: String -> String -> IO ()
writeTextFile fileName !contents = do
    handle <- openFile fileName WriteMode
    hPutStr handle contents
    hClose handle      

writeHostFile :: String -> Either ParseError String -> IO ()
writeHostFile _ (Left e) = print $ show e
writeHostFile fileName (Right !content) = writeTextFile fileName content


data Line = Empty | Comment String | Command String String

instance Show Line where
    show Empty = ""
    show (Comment a) = '#' : a
    show (Command a b) = a ++ " " ++ b


transform :: InOrOut -> Line -> Line
transform inOrOut (Command a b) = 
    let [check, replace] = getParams inOrOut in
        if a == check then Command replace b else 
            Command a b
    where
        getParams InOffice = ["80.227.47.62", "192.168.1.42"]
        getParams OutOffice = reverse $ getParams InOffice
transform _ a = a


document :: Parser [Line]
document = line `sepEndBy` void (char '\n')

line :: Parser Line
line = try comment <|> try command <|> emptyLine

comment :: Parser Line
comment = Comment <$> do
    void $ char '#'
    many (noneOf "\n")
    --anyChar `manyTill` char '\n'

command :: Parser Line
command = Command <$> ip <*> host
    where 
        ip = lexeme (try ip4 <|> ip6)
        host = many1 (noneOf "\n")

emptyLine :: Parser Line
emptyLine = const Empty <$> void (many1 (oneOf "\t ") <|> string "")

ip4 :: Parser String
ip4 = do
    ips <- ipdigit `sepBy1` char '.'
    return $ intercalate "." ips
    where 
        ipdigit = do 
            i <- many1 digit
            let j = read i :: Integer in
                if j >= 0 && j < 256 then return i else fail "Expected a number less than 256"

ip6 :: Parser String
ip6 = string "::1" <|> do
    ips <- ipdigit `sepBy1` string "::"
    return $ intercalate "::" ips
    where
        ipdigit = many1 (noneOf "\n\t: ")