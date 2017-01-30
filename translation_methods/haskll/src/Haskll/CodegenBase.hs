{-# LANGUAGE TemplateHaskell #-}

module Haskll.CodegenBase where

import           Control.Lens             (makeLenses, uses, (%=), (^.))
import qualified Data.Map                 as M
import           Universum

import           Haskll.Syntax.Expression (TokenExp (..))
import           Haskll.Types             (BindType (..), ProdItem (..), Token)

data HaskllState = HaskllState
    { _sourceString :: [Token]
    } deriving Show

makeLenses ''HaskllState

newtype HParser a = HParser
    { getHParser :: State HaskllState a
    } deriving (Functor, Applicative, Monad, MonadState HaskllState)

peekToken :: HParser (Maybe Token)
peekToken = uses sourceString head

consumeToken :: HParser ()
consumeToken = sourceString %= drop 1

data AST = ASTNode Text AST | ASTLeaf Token
type RuleRet = (AST, [(Text,Text)])

init :: Text -> HaskllState
init = undefined
