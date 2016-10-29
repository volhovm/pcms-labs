{-# LANGUAGE TypeOperators #-}

-- | This module defines haskll grammar
module Grammar.Expression
       ( GrammarDef (..)
       , Expression (..)
       , Variant (..)
       , Term (..)
       , TokenExp (..)
       ) where

import           Universum

type Variable = Text
type Code = Text

-- | Definiton of grammar
data GrammarDef = GrammarDef
    { gMembers :: Text
    , gExprs   :: [Expression]
    , gTokens  :: [TokenExp]
    } deriving (Show)

-- | Single grammar expression
data Expression = Expression
    { eName            :: Text
    , eReceivingAttrs  :: [Text]
    , eGeneratingAttrs :: [Text]
    , eLocals          :: [Text]
    , eVariant         :: Term
    } deriving (Show)

-- | Single variant of expression
data Variant = Variant
    { vTerm :: Term
    , vCode :: Maybe Text
    } deriving (Show)

-- | Term of variant
data Term
    = Term :&: Term
    | Term :|: Term
    | (:*:) Term
    | (:?:) Term
    | Variable :+=: Term
    | Variable ::=: Term
    | WithCode Term Code
    | TermString Text
    | TermToken Text
    deriving (Show)

-- | Token expression (definition)
data TokenExp = TokenExp
    { tName  :: Text
    , tRegex :: Text
    } deriving (Show)
