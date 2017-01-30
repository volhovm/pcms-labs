{-# LANGUAGE TemplateHaskell #-}
-- | Common types of grammar

module Haskll.Types
       ( Attributes
       , BindType (..)
       , ProdItem (..)
       , pName
       , pArgs
       , bindVar
       , prettyProdItem
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

prettyProdItem :: ProdItem -> Text
prettyProdItem ProdEpsilon          = "ε"
prettyProdItem (ProdCode _)         = "<code>"
prettyProdItem ProdTerminal {..}    = _pName
prettyProdItem ProdNonterminal {..} = _pName


data GrammarRule = GrammarRule
    { grName            :: Text
    , grProd            :: [ProdItem]
    , grReceivingAttrs  :: Attributes
    , grGeneratingAttrs :: Attributes
    , grLocals          :: Attributes
    } deriving (Show,Eq,Ord)

prettyGrammarRule :: GrammarRule -> Text
prettyGrammarRule GrammarRule{..} =
    grName <> " -> " <> T.intercalate " " (map prettyProdItem grProd)
  where

-- Token. After tokenizing the text is just a list of tokens.
data Token = Token
    { tokenName :: Text
    , tokenText :: Text
    } deriving (Show)
