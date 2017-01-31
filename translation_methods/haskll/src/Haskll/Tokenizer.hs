{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}
-- | Break input string into tokens defined by the grammar

module Haskll.Tokenizer
       ( tokenizeT
       , testTokenizer
       ) where

import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Text.Regex.PCRE          ((=~))
import           Universum

import           Haskll.Syntax.Expression (GrammarDef (..), TokenExp (..))
import           Haskll.Syntax.Parser
import           Haskll.Types             (Token (..))


tokenizeT :: [TokenExp] -> Text -> Either Text [Token]
tokenizeT tokens (T.unpack -> t) =
    (++ [Token "EOF" "EOF"]) <$> tokenize tokens t

tokenize :: [TokenExp] -> [Char] -> Either Text [Token]
tokenize tokenExps input = do
    (t,(match,after)) <- case curTokenExp of
        Nothing -> Left $ "Can't tokenize starting on: " <> T.pack (take 20 input)
        Just x  -> Right x
    next <- if null after then Right [] else tokenize tokenExps after
    pure $ if tSkip t
           then next
           else (Token (tName t) (T.pack match)) : next
  where
    curTokenExp :: Maybe (TokenExp, ([Char],[Char]))
    curTokenExp = head $ catMaybes $ map (\t -> (t,) <$> tokenMatches t) tokenExps
    tokenMatches TokenExp{..} =
        let (before,curMatch,after) = input =~ (T.unpack tRegex)
        in case before of
               ("" :: [Char]) -> Just (curMatch, after)
               _              -> Nothing

testTokenizer :: IO ()
testTokenizer = do
    t <- TIO.readFile "resources/test2.g"
    f <- TIO.readFile "resources/functional1.hll"
    let (Right g) = parseGrammar t
        tokens = either panic identity $ tokenizeT (gTokens g) f
    forM_ (gTokens g) $ print
    forM_ tokens $ \Token{..} -> putText $ tokenName <> ": " <> tokenText
