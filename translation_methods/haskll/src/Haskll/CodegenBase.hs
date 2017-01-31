{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Haskll.CodegenBase where

import           Control.Lens             (makeLenses, uses, (%=), (^.), _1, _2, _3, _4,
                                           _5)
import qualified Data.Map                 as M
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Data.Tree                (Tree (..))
import           Universum

import           Haskll.Syntax.Expression (TokenExp (..))
import           Haskll.Tokenizer         (tokenize)
import           Haskll.Types             (ProdItem (..), Token (..))

data HaskllState = HaskllState
    { _sourceString :: [Token]
    } deriving Show

makeLenses ''HaskllState

newtype HParser a = HParser
    { getHParser :: State HaskllState a
    } deriving (Functor, Applicative, Monad, MonadState HaskllState)

evalHParser :: HaskllState -> HParser a -> a
evalHParser st (HParser action) = evalState action st

peekToken :: HParser (Maybe Token)
peekToken = uses sourceString head

consumeToken :: Text -> HParser Token
consumeToken tokenExpected = do
    tokenReal <- uses sourceString head
    maybe consumeNothing consumeContinue tokenReal
  where
    consumeNothing =
        panic $ "Tried to consume Nothing -- no more tokens left"
    noMatch t =
       panic $ "Couldn't consume token: expected " <>
        show tokenExpected <> ", got " <> show t
    consumeContinue t | tokenName t == tokenExpected = do
        sourceString %= drop 1
        -- traceM $ "Consuming token: " <> tokenExpected
        pure t
    consumeContinue t = noMatch $ tokenName t

data AST
    = ASTNode Text [AST]
    | ASTLeaf Token
    deriving (Eq, Ord, Show)

astToTree :: AST -> Tree String
astToTree (ASTLeaf t)          = Node (T.unpack $ tokenText t) []
astToTree (ASTNode t children) =
    Node (T.unpack t) $
    case map astToTree children of
        [] -> [Node "ε" []]
        x  -> x

readTextUnsafe :: (Read a) => Text -> a
readTextUnsafe k =
    fromMaybe (panic "readTextUnsafe failed") $ readMaybe $ T.unpack k
