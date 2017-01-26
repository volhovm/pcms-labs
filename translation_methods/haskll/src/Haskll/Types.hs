{-# LANGUAGE TemplateHaskell #-}
-- | Common types of grammar

module Haskll.Types
       ( Attributes
       , BindType (..)
       , ProdItem (..)
       , pName
       , pArgs
       , bindVar
       , GrammarRule (..)
       , prettyGrammarRule
       , Token (..)
       ) where

import           Control.Lens (makeLenses)
import qualified Data.Text    as T
import           Universum

type Attributes = [(Text,Text)]

data BindType = BindAdd | BindAssign deriving (Show,Eq,Ord)

-- Producable item on the rhs of grammar rule.
data ProdItem
    = ProdTerminal { _pName   :: Text
                   , _bindVar :: Maybe (Text, BindType)
                   }
    | ProdCode Text
    | ProdEpsilon
    | ProdNonterminal { _pName   :: Text
                      , _pArgs   :: Maybe Text
                      , _bindVar :: Maybe (Text, BindType)
                      }
    deriving (Show,Eq,Ord)

makeLenses ''ProdItem

data GrammarRule = GrammarRule
    { gName            :: Text
    , gProd            :: [ProdItem]
    , gReceivingAttrs  :: Attributes
    , gGeneratingAttrs :: Attributes
    , gLocals          :: Attributes
    } deriving (Show,Eq,Ord)

prettyGrammarRule :: GrammarRule -> Text
prettyGrammarRule GrammarRule{..} =
    gName <> " -> " <> T.intercalate " " (map prettyItem gProd)
  where
    prettyItem ProdEpsilon          = "ε"
    prettyItem (ProdCode _)         = "<code>"
    prettyItem ProdTerminal {..}    = _pName
    prettyItem ProdNonterminal {..} = _pName

-- Token. After tokenizing the text is just a list of tokens.
data Token = Token
    { tokenName :: Text
    , tokenText :: Text
    } deriving (Show)
