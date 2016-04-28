module Recparser.Parser
       ( PA (..)
       , PB (..)
       , PC (..)
       , PD (..)
       , runParser
       ) where

import           Recparser.Lexer      (Token (..))

import           Control.Monad.Except (ExceptT (..), catchError, runExceptT,
                                       throwError)
import           Control.Monad.State  (State, get, modify, put, runState)

type Parser a = ExceptT String (State [Token]) a

{-
A → n B | n
B → A C
C → * D | + D | - D
D → ε | B
-}

data PA = PAPrefix Integer PB
        | PANum Integer
data PB = PB PA PC
data PC = (:+) PD
        | (:-) PD
        | (:*) PD
data PD = PDε | PDB PB

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
            throwError $ "Expected integer, got: " ++ show t

parsePB :: Parser PB
parsePB = PB <$> parsePA <*> parsePC

parsePC :: Parser PC
parsePC = do
    opToken <- popToken
    d <- parsePD
    case opToken of
        (:+:) -> return $ (:+) d
        (:-:) -> return $ (:-) d
        (:*:) -> return $ (:*) d
        t -> do
            pushToken t
            throwError ("Expected +,- or *, got: " ++ show t)

parsePD :: Parser PD
parsePD = do
    bBoxed <- (Just <$> parsePB) `catchError` const (return Nothing)
    return $ maybe PDε PDB bBoxed

runParser :: [Token] -> Either String PA
runParser tokens =
    let (a,lefttokens) = runState (runExceptT parsePA) tokens
    in if null lefttokens
           then a
           else Left "Couldn't parse all input."
