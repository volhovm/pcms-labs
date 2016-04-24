-- | Reexporting module

module Recparser
       ( module Exports
       , parseGrammar
       ) where

import           Recparser.Lexer       as Exports
import           Recparser.Parser      as Exports

import           Data.Bifunctor        (first)
import qualified Data.ByteString.Char8 as BS

parseGrammar :: BS.ByteString -> Either String PA
parseGrammar str = do
    tokens <- (("Lexer failed: " ++) . show) `first` lexicalAnalyzer str
    (("Parser failed: " ++) . show) `first` runParser tokens
