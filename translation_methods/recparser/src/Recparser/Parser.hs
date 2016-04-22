module Recparser.Parser
       ( PA (..)
       , PB (..)
       , runParser
       ) where

import           Recparser.Lexer      (Token (..))

import           Control.Monad.Except (ExceptT (..), catchError, runExceptT,
                                       throwError)
import           Control.Monad.State  (State, evalState, get, modify, put)


type Parser a = ExceptT String (State [Token]) a

data PA = PAPrefix Integer PB
        | PANum Integer

data PB = (:+) PA
        | (:-) PA
        | (:*) PA
        | (:++) PA PB
        | (:--) PA PB
        | (:**) PA PB

popToken :: Parser Token
popToken = do
    tokens <- get
    case tokens of
        [] -> throwError "Unexpected end of input"
        x:xs -> put xs >> return x

pushToken :: Token -> Parser ()
pushToken t = modify (t :)

parsePA :: Parser PA
parsePA = do
    number <- popToken
    case number of
        (TNum n) ->
            (PAPrefix n <$> parsePB) `catchError` const (return (PANum n))
        t -> do
            pushToken number
            throwError $ "Expected number, got other stuff: " ++ show t

parsePB :: Parser PB
parsePB = do
    a <- parsePA
    opToken <- popToken
    bBoxed <- (Just <$> parsePB) `catchError` const (return Nothing)
    case (opToken, bBoxed) of
        ((:+:),Nothing) -> return $ (:+) a
        ((:-:),Nothing) -> return $ (:-) a
        ((:*:),Nothing) -> return $ (:*) a
        ((:+:),Just b) -> return $ (:++) a b
        ((:-:),Just b) -> return $ (:--) a b
        ((:*:),Just b) -> return $ (:**) a b
        (t,_) -> do
            pushToken t
            throwError ("Expected +,- or *, got: " ++ show t)

runParser :: [Token] -> Either String PA
runParser = evalState (runExceptT parsePA)
