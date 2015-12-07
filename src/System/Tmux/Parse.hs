module System.Tmux.Parse where

import Text.ParserCombinators.Parsec

parseRow :: Parser [String]
parseRow = (many1 $ noneOf "\t\n") `sepBy` char '\t'

parseTable :: [String] -> Parser [[(String, String)]]
parseTable vars =
  do lines <- parseRow `sepBy` char '\n'
     return $ map (zip vars) $ filter (not . null) lines

parseOutput vars = parse (parseTable vars) "(unknown)"

