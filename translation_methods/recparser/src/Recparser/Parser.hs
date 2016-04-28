{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}

module Recparser.Parser
       ( PA (..)
       , PB (..)
       , PC (..)
       , PD (..)
       , runParser
       ) where

import           Recparser.Lexer      (Token (..))

import           Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import           Control.Monad.State  (State, get, put, runState)

type Parser a = ExceptT String (State [Token]) a

{-
A → n D
B → A C
C → * D | + D | - D
D → ε | B
-}

data PA = PA Integer PD
data PB = PB PA PC
data PC = (:+) PD
        | (:-) PD
        | (:*) PD
data PD = PDε | PDB PB

popToken :: Parser ()
popToken = do
    tokens <- get
    case tokens of
        [] -> throwError "Unexpected end of input"
        _:xs -> put xs

peekToken :: Parser (Maybe Token)
peekToken = do
    tokens <- get
    case tokens of
        [] -> return Nothing
        x:_ -> return (Just x)

failExpected :: String -> Maybe Token -> Parser a
failExpected expected (Just e) = throwError $
    "Expected " ++ expected ++ ", got " ++ show e
failExpected expected Nothing = throwError $
    "Expected " ++ expected ++ ", got epsilon (EOF)"

parsePA :: Parser PA
parsePA = do
    number <- peekToken
    case number of
        (Just (TNum n)) -> popToken >> PA n <$> parsePD
        t -> failExpected "integer while parsing PA" t

parsePB :: Parser PB
parsePB = do
    token <- peekToken
    case token of
        (Just (TNum _)) -> PB <$> parsePA <*> parsePC
        t -> failExpected "integer while parsing PB" t

parsePC :: Parser PC
parsePC = do
    opToken <- peekToken
    ret <- case opToken of
        Just (:+:) -> return (:+)
        Just (:-:) -> return (:-)
        Just (:*:) -> return (:*)
        t -> failExpected "+, - or *" t
    popToken
    ret <$> parsePD

parsePD :: Parser PD
parsePD = do
    tokens <- peekToken
    case tokens of
        Just (TNum _) -> PDB <$> parsePB
        _ -> return PDε

runParser :: [Token] -> Either String PA
runParser tokens =
    case runState (runExceptT parsePA) tokens of
        (o@(Left _),_) -> o
        (a, []) -> a
        (_, _) -> Left "Couldn't parse all input"
