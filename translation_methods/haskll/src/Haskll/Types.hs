-- | Common types of grammar

module Haskll.Types where

import           Universum

-- Token. After tokenizing the text is just a list of tokens.
data Token = Token
    { tokenName :: Text
    , tokenText :: Text
    } deriving Show
