module Main (main) where

import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Universum

import           Haskll.Codegen       (genParser)
import           Haskll.Syntax.Parser (parseGrammar)

main :: IO ()
main = do
    args <- getArgs
    let inputFile = case args of
          [x] -> x
          _   -> "resources/functional.g"
    putText $ "Parsing grammar " <> T.pack inputFile
    g <- parseGrammar <$> TIO.readFile inputFile
    putText "Parsing grammar done, generating parser"
    TIO.writeFile "HaskllParser.hs" $ genParser $ either onLeft identity g
    putText "Done, exiting"
  where
    onLeft x = panic $ "Could not parse: " <> x
