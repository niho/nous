module Main where

import Parse.Parse
import System.Environment
import System.IO
import Control.Monad
import GHC.IO.Encoding (setLocaleEncoding, utf8)


main :: IO ()
main = do
  setLocaleEncoding utf8

  allArgs <- getArgs

  case allArgs of
    [] ->
      putStrLn "Usage: nous [FILES...]"

    files -> do
      mapM_ parseFile files

parseFile :: String -> IO ()
parseFile file = do
  src <- readFile file
  case parseSource src of
    Left e -> print e >> fail "parse error"
    Right r -> print r
