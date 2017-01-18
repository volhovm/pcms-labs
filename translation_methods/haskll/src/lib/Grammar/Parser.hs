{-# LANGUAGE TemplateHaskell #-}

-- | Module that provides parsing capabilities for haskll grammar
module Grammar.Parser () where

import qualified Data.ByteString      as BS
import           Data.Char            (isAlphaNum, isLower, isSpace, isUpper)
import           Data.FileEmbed       (embedStringFile)
import qualified Data.Text            as T
import           Text.Megaparsec      (between, eof, parse, parseTest, runParser, satisfy,
                                       sepEndBy, string, try)
import           Text.Megaparsec.Char (newline, space)
import           Text.Megaparsec.Prim (MonadParsec)
import           Text.Megaparsec.Text (Parser)
import           Universum

import           Grammar.Expression   (Expression (..), GrammarDef (..), Term (..),
                                       TokenExp (..))

type String = [Char]

t :: Text
t = $(embedStringFile "resources/test1.g")

----------------------------------------------------------------------------
-- Re-written combinators
----------------------------------------------------------------------------

-- These two use "try" in contrast to library versions.
sepBy :: MonadParsec e s m => m a -> m sep -> m [a]
sepBy1 :: MonadParsec e s m => m a -> m sep -> m [a]
sepBy p sep = sepBy1 p sep <|> pure []
sepBy1 p sep = (:) <$> p <*> many (try $ sep *> p)

post :: (Monad m) => m a -> m b -> m a
post f b = f >>= \a -> b $> a

lexem :: Parser a -> Parser a
lexem a = (try space >> a) `post` (try space)

(&^&) :: (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
(&^&) a b = \x -> a x && b x

----------------------------------------------------------------------------
-- Grammar parsing
----------------------------------------------------------------------------

tokens :: Parser [TokenExp]
tokens = lexem $ token `sepBy` space

wordUpper, wordCamel :: Parser String
wordUpper = some $ satisfy $ isUpper &^& isAlphaNum
wordCamel = do
    hd <- try $ satisfy $ isLower &^& isAlphaNum
    tl <- some $ satisfy isAlphaNum
    pure $ hd:tl

token :: Parser TokenExp
token = try $ do
    name <- wordUpper
    void $ lexem $ string ":"
    skip <- optional $ string "SKIP"
    void $ lexem $ string "/"
    regex <- some $ satisfy (/= '/')
    void $ string "/" >> lexem (string ";")
    pure $ TokenExp (T.pack name) (T.pack regex) (isJust skip)

grammarDef = do
    gExprs <- many expression
    gTokens <- many token
    pure $ GrammarDef "" gExprs gTokens

expression = do
    eName <- T.pack <$> lexem wordCamel
    void $ lexem $ string ":"
    eTerm <- term
    void $ lexem $ string ";"
    pure $ Expression eName [] [] [] eTerm

term = termAlt
  where
    termAlt, termString :: Parser Term
    termAlt = foldr1 (:|:) <$> lexem (termAnd `sepBy` string "|")
    termAnd = foldr1 (:&:) <$> many (try $ lexem $ termString <|> termToken)
    termString =
        TermString . T.pack <$>
        between (try $ string "'") (string "'") (many $ satisfy (/= '\''))
    termToken = TermToken . T.pack <$> try wordCamel

--pTokenExp :: Parsec TokenExp
--pTokenExp = notImplemented
--
--parseFile :: BS.ByteString -> GrammarDef
--parseFile = notImplemented

kek :: Parser String
kek = string "kek"
