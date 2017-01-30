{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskll.CodegenBase where

import           Control.Lens             (makeLenses, uses, (%=), (^.))
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

consumeToken :: Text -> HParser AST
consumeToken tokenExpected = do
    tokenReal <- uses sourceString head
    if fmap tokenName tokenReal == Just tokenExpected
    then do
        sourceString %= drop 1
        -- traceM $ "Consuming token: " <> tokenExpected
        pure $ maybe ASTLeafEps ASTLeaf tokenReal
    else panic $ "Couldn't consume token " <> show tokenExpected

data AST
    = ASTNode Text [AST]
    | ASTLeaf Token
    | ASTLeafEps
    deriving (Eq, Ord, Show)

astToTree :: AST -> Tree String
astToTree ASTLeafEps           = Node "ε" []
astToTree (ASTLeaf t)          = Node (T.unpack $ tokenText t) []
astToTree (ASTNode t children) = Node (T.unpack t) $ map astToTree children
