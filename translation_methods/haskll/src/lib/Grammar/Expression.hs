{-# LANGUAGE TypeOperators #-}

-- | This module defines haskll grammar
module Grammar.Expression
       ( GrammarDef (..)
       , Expression (..)
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
    , eTerm            :: Term
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
    | Subterm Text
    deriving (Show)

-- | Token expression (definition)
data TokenExp = TokenExp
    { tName  :: Text
    , tRegex :: Text
    , tSkip  :: Bool
    } deriving (Show)
