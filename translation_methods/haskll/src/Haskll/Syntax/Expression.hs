{-# LANGUAGE TypeOperators #-}

-- | This module defines haskll grammar
module Haskll.Syntax.Expression
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
    { gName    :: Text
    , gImports :: Maybe Text
    , gMembers :: Maybe Text
    , gExprs   :: [Expression]
    , gTokens  :: [TokenExp]
    } deriving (Show)

-- | Single grammar expression
data Expression = Expression
    { eName            :: Text
    , eReceivingAttrs  :: [(Text,Text)] -- (type,varname)
    , eGeneratingAttrs :: [(Text,Text)]
    , eLocals          :: [(Text,Text)]
    , eTerm            :: Term
    } deriving (Show)

-- | Term of variant
data Term
    = Term :&: Term
    | Term :|: Term
    | (:*:) Term
    | (:+:) Term
    | (:?:) Term
    | Variable :+=: Term
    | Variable ::=: Term
    | WithCode Term Code
    | TermString Text
    | TermToken Text
    | TermOther Text (Maybe Code)
    | Subterm Term
    deriving (Show)

-- | Token expression (definition)
data TokenExp = TokenExp
    { tName  :: Text
    , tRegex :: Text
    , tSkip  :: Bool
    } deriving (Show)
