import Text.ParserCombinators.Parsec

{- A CSV file contains 0 or more lines, each of which is terminated
   by the end-of-line character (eol). -}
csvFile :: GenParser Char st [[String]]
csvFile = 
    do result <- many line
       eof
       return result

-- Each line contains 1 or more cells, separated by a comma
line :: GenParser Char st [String]
line = 
    commentChar <|>
    do result <- cells
       eol                       -- end of line
       return result

commentChar :: GenParser Char st [String]
commentChar =
    do 
      _ <- char '#'
      _ <- many (noneOf "\n")
      return []
       
-- Build up a list of cells.  Try to parse the first cell, then figure out 
-- what ends the cell.
cells :: GenParser Char st [String]
cells = 
    do first <- cellContent
       next <- remainingCells
       return (first : next)

-- The cell either ends with a comma, indicating that 1 or more cells follow,
-- or it doesn't, indicating that we're at the end of the cells for this line
remainingCells :: GenParser Char st [String]
remainingCells =
    (char ',' >> cells)            -- Found comma?  More cells coming
    <|> return []                -- No comma?  Return [], no more cells

-- Each cell contains 0 or more characters, which must not be a comma or
-- EOL
cellContent :: GenParser Char st String
cellContent = 
    many (noneOf ",\n")
       

-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"
-- parseCSV input = parse csvFile "(unknown)" input


main :: IO ()
main = do
  x <- getContents
  case parseCSV x of
    Left e -> print e
    Right r -> mapM_ print r