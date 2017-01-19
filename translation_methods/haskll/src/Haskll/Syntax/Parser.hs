{-# LANGUAGE TemplateHaskell #-}

-- | Module that provides parsing capabilities for haskll grammar
module Haskll.Syntax.Parser (parseGrammar) where

import           Data.Char                (isAlphaNum, isLower, isUpper)
import qualified Data.Text                as T
import           Text.Megaparsec          (between, choice, eof, parse, satisfy, string,
                                           try)
import           Text.Megaparsec.Char     (newline, space)
import           Text.Megaparsec.Error    (parseErrorPretty)
import           Text.Megaparsec.Prim     (MonadParsec)
import           Text.Megaparsec.Text     (Parser)
import           Universum

import           Haskll.Syntax.Expression (Expression (..), GrammarDef (..), Term (..),
                                           TokenExp (..))

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

spaceCmt :: Parser ()
spaceCmt = space >> void (optional $ comment >> space)
  where
    comment = string "//" >> many (satisfy (/= '\n')) >> newline

lexem :: Parser a -> Parser a
lexem a = (try spaceCmt >> a) `post` (try spaceCmt)

(&^&), (|^|) :: (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
(&^&) a b = \x -> a x && b x
(|^|) a b = \x -> a x || b x

betweenMatching :: Char -> Char -> Parser Text
betweenMatching l r =
    fmap T.pack $ try $ between (char' l) (char' r) betwMatchGo
  where
    char' :: Char -> Parser [Char]
    char' c = (:[]) <$> satisfy (== c)
    nonParenth = some $ satisfy $ (/= r) &^& (/= l)
    parenthed x = mconcat <$> sequence [char' l, x, char' r]
    betwMatchGo =
        fmap mconcat $
        many $ try (parenthed nonParenth) <|> nonParenth <|> try (parenthed betwMatchGo)

-- Behaves like optional, but retruns empty list on Nothing
optList :: Parser [a] -> Parser [a]
optList p = fromMaybe [] <$> optional p

-- kek,tst :: Parser String
-- kek = string "kek"
-- tst = try $ kek `post` string "*"

----------------------------------------------------------------------------
-- Grammar parsing
----------------------------------------------------------------------------

wordUpper, wordCamel, wordConstructor :: Parser Text
wordUpper = fmap T.pack $ some $ satisfy $ isUpper &^& isAlphaNum
wordCamel = do
    hd <- try $ satisfy $ isLower &^& isAlphaNum
    tl <- many $ satisfy isAlphaNum
    pure $ T.pack $ hd:tl
wordConstructor = do
    hd <- try $ satisfy $ (isUpper &^& isAlphaNum) |^| (`elem` ("[]"::[Char]))
    tl <- many $ satisfy $ isAlphaNum |^| (`elem` ("[]."::[Char]))
    pure $ T.pack $ hd:tl

grammarDef :: Parser GrammarDef
grammarDef = do
    gName <- lexem (string "grammar") >> (wordConstructor `post` (string ";"))
    gImports <- optional $ try $ lexem (string "@imports") >> lexem (betweenMatching '{' '}')
    gMembers <- optional $ try $ lexem (string "@members") >> lexem (betweenMatching '{' '}')
    gExprs <- many expression
    gTokens <- tokens
    eof
    pure $ GrammarDef {..}

expression :: Parser Expression
expression = do
    eName <- lexem wordCamel
    recv <- optList $ lexem $ receiveParams
    gen <- optList $ lexem $ genParams
    void $ lexem $ string ":"
    eTerm <- term
    void $ lexem $ string ";"
    pure $ Expression eName recv gen [] eTerm
  where
    paramArg :: Parser (Text, Text) -- (type, name)
    paramArg = do
        paramType <- wordConstructor
        spaceCmt
        paramName <- wordCamel
        pure $ (paramType,paramName)
    paramList :: Parser [(Text,Text)]
    paramList =
        between (string "[") (string "]") $
        lexem $ (lexem paramArg) `sepBy1` string ","
    receiveParams = paramList
    genParams = string "returns" >> spaceCmt >> paramList

term :: Parser Term
term = termAlt
  where
    termAlt =
        foldr1 (:|:) <$> lexem (lexem (withCode <|> termAnd) `sepBy1` string "|")
    withCode = try $ do
        predTerm <- lexem termAnd
        codePart <- betweenMatching '{' '}'
        pure $ WithCode predTerm codePart
    termAnd = foldr1 (:&:) <$> some (try $ lexem $ assoc1)
    assoc1 =
        choice $ map lexem $
        [withVar, postModifier, assoc0]
    assoc0 =
        choice $ map lexem $
        [termString, termToken, subterm, termOther]
    withVar = try $ do
        varName <- wordCamel
        binding <-
            lexem $
            (string "+=" >> pure (:+=:)) <|>
            (string "="  >> pure (::=:)) <|>
            (string ":=" >> pure (::=:))
        subt <- assoc0
        pure $ binding varName subt
    termString =
        TermString . T.pack <$>
        between (try $ string "'") (string "'") (many $ satisfy (/= '\''))
    termToken = TermToken <$> try wordUpper
    termOther = try $ do
        otherName <- try wordCamel
        callParams <- optional (betweenMatching '[' ']')
        pure $ TermOther otherName callParams
    parens = between (string "(") (string ")") (lexem term)
    subterm = Subterm <$> parens
    postModifier = try $ do
        subt <- assoc0
        modifier <- lexem $ choice [
              (string "*" >> pure (:*:))
            , (string "+" >> pure (:+:))
            , (string "?" >> pure (:?:))
            ]
        pure $ modifier subt

tokens :: Parser [TokenExp]
tokens = lexem $ token `sepBy` spaceCmt

token :: Parser TokenExp
token = try $ do
    name <- wordUpper
    void $ lexem $ string ":"
    skip <- optional $ string "SKIP"
    void $ lexem $ string "/"
    regex <- T.strip . T.pack <$> some (satisfy (/= ';'))
    unless (T.last regex == '/') $ fail "Last symbol of regex should be /"
    void $ lexem (string ";")
    pure $ TokenExp name (T.dropEnd 1 regex) (isJust skip)

----------------------------------------------------------------------------
-- Launching
----------------------------------------------------------------------------

parseGrammar :: Text -> Either Text GrammarDef
parseGrammar s =
    first (T.pack . parseErrorPretty) $
    parse grammarDef "Grammar definition" s
