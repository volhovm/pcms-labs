-- | This module defines haskll grammar
module Grammar.Expression
       ( GrammarDef (..)
       , Expression (..)
       , Variant (..)
       , Term (..)
       , TokenExp (..)
       ) where

import           Universum

-- | Definiton of grammar
data GrammarDef = GrammarDef
    { gMembers :: Text
    , gExprs   :: [Expression]
    , gTokens  :: [TokenExp]
    }

-- | Single grammar expression
data Expression = Expression
    { eName            :: Text
    , eReceivingAttrs  :: [Text]
    , eGeneratingAttrs :: [Text]
    , eLocals          :: [Text]
    , eVariants        :: [Variant]
    }

-- | Single variant of expression
data Variant = Variant
    { vTerm :: Term                -- ^ Variant
    , vCode :: Maybe Text        -- ^ Code for it
    }

-- | Term of variant
data Term = Conc [Term]            -- ^ Term concatenation
          | TermS Text           -- ^ Other rule call with maybe params
                  (Maybe [Text])
          | TermL Text           -- ^ String literal
          | TermT Text           -- ^ Token

-- | Token expression (definition)
data TokenExp = TokenExp
   { tName  :: Text
   , tRegex :: Text
   }
