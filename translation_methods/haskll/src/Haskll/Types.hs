{-# LANGUAGE TemplateHaskell #-}
-- | Common types of grammar

module Haskll.Types
       ( Attributes
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

-- Producable item on the rhs of grammar rule.
data ProdItem
    = ProdTerminal { _pName   :: Text
                   , _bindVar :: Maybe Text
                   }
    | ProdCode Text
    | ProdEpsilon
    | ProdNonterminal { _pName   :: Text
                      , _pArgs   :: Maybe Text
                      , _bindVar :: Maybe Text
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
    , grProds           :: [[ProdItem]]
    , grReceivingAttrs  :: Attributes
    , grGeneratingAttrs :: Attributes
    , grLocals          :: Attributes
    } deriving (Show,Eq,Ord)

prettyGrammarRule :: GrammarRule -> Text
prettyGrammarRule GrammarRule{..} =
    grName <> " -> " <>
    T.intercalate " | " (map (T.intercalate " " . map prettyProdItem) grProds)

-- Token. After tokenizing the text is just a list of tokens.
data Token = Token
    { tokenName :: Text
    , tokenText :: Text
    } deriving (Eq,Ord,Show)
