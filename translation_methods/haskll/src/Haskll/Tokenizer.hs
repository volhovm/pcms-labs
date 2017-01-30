{-# LANGUAGE FlexibleContexts #-}
-- | Break input string into tokens defined by the grammar

module Haskll.Tokenizer
       ( tokenize
       , tokenizeT
       ) where

import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Text.Regex.PCRE          ((=~))
import           Universum

import           Haskll.Syntax.Expression (GrammarDef (..), TokenExp (..))
import           Haskll.Syntax.Parser
import           Haskll.Types             (Token (..))


tokenizeT :: [TokenExp] -> Text -> Either Text [Token]
tokenizeT tokens = tokenize tokens . T.unpack

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

kek  = do
    t <- TIO.readFile "resources/test2.g"
    f <- TIO.readFile "resources/simple.sf"
    let (Right g) = parseGrammar t
    print $ gTokens g
--        (Right tokens) = tokenize (gTokens g) (T.unpack f)
--    forM_ tokens $ \Token{..} -> putText $ tokenName <> ": " <> tokenText

gtkns :: [TokenExp]
gtkns = [TokenExp {tName = "WHITESPACE", tRegex = "[ \\t]+", tSkip = True},TokenExp {tName = "HOLE", tRegex = "_", tSkip = False},TokenExp {tName = "TRUE", tRegex = "True", tSkip = False},TokenExp {tName = "FALSE", tRegex = "False", tSkip = False},TokenExp {tName = "ARROW", tRegex = "->|\8594", tSkip = False},TokenExp {tName = "DOUBLECOLON", tRegex = "::", tSkip = False},TokenExp {tName = "PURETYPE", tRegex = "(Int|Bool|Char)", tSkip = False},TokenExp {tName = "VERTBAR", tRegex = "\\|", tSkip = False},TokenExp {tName = "EQUALS", tRegex = "=", tSkip = False},TokenExp {tName = "COMMA", tRegex = ",", tSkip = False},TokenExp {tName = "LET", tRegex = "let", tSkip = False},TokenExp {tName = "IF", tRegex = "if", tSkip = False},TokenExp {tName = "THEN", tRegex = "then", tSkip = False},TokenExp {tName = "ELSE", tRegex = "else", tSkip = False},TokenExp {tName = "PARENL", tRegex = "\\(", tSkip = False},TokenExp {tName = "PARENR", tRegex = "\\)", tSkip = False},TokenExp {tName = "PARENSQL", tRegex = "\\[", tSkip = False},TokenExp {tName = "PARENSQR", tRegex = "\\]", tSkip = False},TokenExp {tName = "INTLIT", tRegex = "[+-]?[0-9]+", tSkip = False},TokenExp {tName = "INFIX", tRegex = "([\\+\\-\\*\\<\\>\\^\\/] | '==' | '/=' | '<=' | '>=')", tSkip = False},TokenExp {tName = "NAME", tRegex = "[a-z][a-zA-Z\\'_0-9]*", tSkip = False},TokenExp {tName = "CHARLIT", tRegex = "[\\'][a-zA-Z0-9]?[\\']", tSkip = False},TokenExp {tName = "STRLIT", tRegex = "[\\\"][a-zA-Z0-9]*[\\\"]", tSkip = False},TokenExp {tName = "NL", tRegex = "[\\r\\n]+", tSkip = False}]
