{-# LANGUAGE TemplateHaskell #-}

-- | Module that provides parsing capabilities for haskll grammar
module Grammar.Parser (grammarDef) where

import qualified Data.ByteString      as BS
import           Data.Char            (isAlphaNum, isLower, isSpace, isUpper)
import           Data.FileEmbed       (embedStringFile)
import qualified Data.Text            as T
import           Text.Megaparsec      (between, choice, eof, parse, parseTest, runParser,
                                       satisfy, sepEndBy, string, try)
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

betweenMatching :: Char -> Char -> Parser String
betweenMatching l r = try $ between (char' l) (char' r) betwMatchGo
  where
    char' :: Char -> Parser String
    char' c = (:[]) <$> satisfy (== c)
    nonParenth = some $ satisfy $ (/= r) &^& (/= l)
    parenthed x = mconcat <$> sequence [char' l, x, char' r]
    betwMatchGo =
        fmap mconcat $
        many $ try (parenthed nonParenth) <|> nonParenth <|> try (parenthed betwMatchGo)

----------------------------------------------------------------------------
-- Grammar parsing
----------------------------------------------------------------------------

tokens :: Parser [TokenExp]
tokens = lexem $ token `sepBy` space

wordUpper, wordCamel :: Parser String
wordUpper = some $ satisfy $ isUpper &^& isAlphaNum
wordCamel = do
    hd <- try $ satisfy $ isLower &^& isAlphaNum
    tl <- many $ satisfy isAlphaNum
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

kek,tst :: Parser String
kek = string "kek"
tst = try $ kek `post` string "*"

term = withCode <|> termAlt
  where
    withCode = try $ do
        predTerm <- lexem termAlt
        codePart <- T.pack <$> betweenMatching '{' '}'
        pure $ WithCode predTerm codePart
    termAlt = foldr1 (:|:) <$> lexem (termAnd `sepBy1` string "|")
    termAnd = foldr1 (:&:) <$> some (try $ lexem $ assoc1)
    assoc1 =
        choice $ map lexem $
        [subMany, subOptional, withVar, assoc0]
    assoc0 =
        choice $ map lexem $
        [termString, termToken, subterm, termOther]
    withVar = try $ do
        varName <- T.pack <$> wordCamel
        binding <-
            lexem $
            (string "+=" >> pure (:+=:)) <|>
            (string ":=" >> pure (::=:))
        subt <- assoc0
        pure $ binding varName subt
    termString =
        TermString . T.pack <$>
        between (try $ string "'") (string "'") (many $ satisfy (/= '\''))
    termToken = TermToken . T.pack <$> try wordUpper
    termOther = TermOther . T.pack <$> try wordCamel
    parens = between (string "(") (string ")") (lexem term)
    subterm = Subterm <$> parens
    subOptional = try $ ((:?:) <$> assoc0) `post` string "?"
    subMany = try $ ((:*:) <$> assoc0) `post` string "*"
