{-# LANGUAGE TypeOperators #-}

-- | This module defines haskll grammar
module Haskll.Syntax.Expression
       ( GrammarDef (..)
       , Expression (..)
       , Term (..)
       , TokenExp (..)
       ) where

import           Haskll.Types (Attributes)
import           Universum

type Variable = Text
type Code = Text

-- | Definiton of grammar
data GrammarDef = GrammarDef
    { gName    :: Text
    , gImports :: Maybe Text
    , gMembers :: Maybe Text
    , gExprs   :: [Expression]
    , gTokens  :: [TokenExp]
    } deriving (Show)

-- | Single grammar expression
data Expression = Expression
    { eName            :: Text
    , eReceivingAttrs  :: Attributes
    , eGeneratingAttrs :: Attributes
    , eLocals          :: Attributes
    , eTerm            :: Term
    } deriving (Show)

-- | Term of variant
data Term
    = Term :&: Term
    | Term :|: Term
    | (:*:) Term
    | (:+:) Term
    | (:?:) Term
    | Variable ::=: Term
    | WithCode Term Code
    | TermToken Text
    | TermOther Text (Maybe Code)
    | Subterm Term
    | TermEpsilon
    deriving (Show,Eq,Ord)

-- | Token expression (definition)
data TokenExp = TokenExp
    { tName  :: Text
    , tRegex :: Text
    , tSkip  :: Bool
    } deriving (Show)
