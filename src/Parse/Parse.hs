module Parse.Parse (parseSource) where

import Control.Applicative ((<$), (<*), (*>), liftA)
import Text.Parsec
import Data.Char

noem :: Parsec String () [[String]]
noem = line `endBy` newline <* eof
  where line = cell `sepBy1` (char ',')
        cell = many $ noneOf ",\n"

parseSource :: String -> Either ParseError [[String]]
parseSource = parse noem "(unknown)"
