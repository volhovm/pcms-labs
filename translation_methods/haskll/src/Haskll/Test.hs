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

----------------------------------------------------------------------------
-- First/follow
----------------------------------------------------------------------------

{-
_gen0 -> "NL"
_gen10 -> "FALSE", "TRUE", "STRLIT", "CHARLIT", "INTLIT"
_gen11 -> "NL", "ε"
_gen2 -> "NAME"
_gen4 -> "NL"
_gen6 -> "NAME", "HOLE"
_gen8 -> "PARENSQL", "NAME", "FALSE", "TRUE", "STRLIT", "CHARLIT", "INTLIT"
_genComb1 -> "NL", "ε"
_genComb12 -> "NL", "ε"
_genComb3 -> "NAME", "ε"
_genComb5 -> "NL"
_genComb7 -> "NAME", "HOLE", "ε"
_genComb9 -> "PARENSQL", "NAME", "FALSE", "TRUE", "STRLIT", "CHARLIT", "INTLIT"
args -> "NAME", "HOLE", "ε"
decl -> "NAME"
func -> "NAME"
primValue -> "PARENSQL", "NAME", "FALSE", "TRUE", "STRLIT", "CHARLIT", "INTLIT"
pureType -> "PARENSQL", "PURETYPE"
retterm -> "NAME", "PARENSQL", "FALSE", "TRUE", "STRLIT", "CHARLIT", "INTLIT", "LET", "PARENL", "IF"
term -> "NAME", "PARENSQL", "FALSE", "TRUE", "STRLIT", "CHARLIT", "INTLIT", "LET", "PARENL", "IF"
topLevel -> "NL", "ε"
type -> "PARENSQL", "PURETYPE", "PARENL"
-}

firstGen :: Map Text [ProdItem]
firstGen = M.fromList [("_gen0",[ProdTerminal {_pName = "NL", _bindVar = Nothing}]),("_gen10",[ProdTerminal {_pName = "FALSE", _bindVar = Nothing},ProdTerminal {_pName = "TRUE", _bindVar = Nothing},ProdTerminal {_pName = "STRLIT", _bindVar = Nothing},ProdTerminal {_pName = "CHARLIT", _bindVar = Nothing},ProdTerminal {_pName = "INTLIT", _bindVar = Nothing}]),("_gen11",[ProdTerminal {_pName = "NL", _bindVar = Nothing},ProdEpsilon]),("_gen2",[ProdTerminal {_pName = "NAME", _bindVar = Nothing}]),("_gen4",[ProdTerminal {_pName = "NL", _bindVar = Nothing}]),("_gen6",[ProdTerminal {_pName = "NAME", _bindVar = Just ("a",BindAdd)},ProdTerminal {_pName = "HOLE", _bindVar = Just ("a",BindAdd)}]),("_gen8",[ProdTerminal {_pName = "PARENSQL", _bindVar = Nothing},ProdTerminal {_pName = "NAME", _bindVar = Just ("n",BindAssign)},ProdTerminal {_pName = "FALSE", _bindVar = Nothing},ProdTerminal {_pName = "TRUE", _bindVar = Nothing},ProdTerminal {_pName = "STRLIT", _bindVar = Nothing},ProdTerminal {_pName = "CHARLIT", _bindVar = Nothing},ProdTerminal {_pName = "INTLIT", _bindVar = Nothing}]),("_genComb1",[ProdTerminal {_pName = "NL", _bindVar = Nothing},ProdEpsilon]),("_genComb12",[ProdTerminal {_pName = "NL", _bindVar = Nothing},ProdEpsilon]),("_genComb3",[ProdTerminal {_pName = "NAME", _bindVar = Nothing},ProdEpsilon]),("_genComb5",[ProdTerminal {_pName = "NL", _bindVar = Nothing}]),("_genComb7",[ProdTerminal {_pName = "NAME", _bindVar = Just ("a",BindAdd)},ProdTerminal {_pName = "HOLE", _bindVar = Just ("a",BindAdd)},ProdEpsilon]),("_genComb9",[ProdTerminal {_pName = "PARENSQL", _bindVar = Nothing},ProdTerminal {_pName = "NAME", _bindVar = Just ("n",BindAssign)},ProdTerminal {_pName = "FALSE", _bindVar = Nothing},ProdTerminal {_pName = "TRUE", _bindVar = Nothing},ProdTerminal {_pName = "STRLIT", _bindVar = Nothing},ProdTerminal {_pName = "CHARLIT", _bindVar = Nothing},ProdTerminal {_pName = "INTLIT", _bindVar = Nothing}]),("args",[ProdTerminal {_pName = "NAME", _bindVar = Just ("a",BindAdd)},ProdTerminal {_pName = "HOLE", _bindVar = Just ("a",BindAdd)},ProdEpsilon]),("decl",[ProdTerminal {_pName = "NAME", _bindVar = Nothing}]),("func",[ProdTerminal {_pName = "NAME", _bindVar = Nothing}]),("primValue",[ProdTerminal {_pName = "PARENSQL", _bindVar = Nothing},ProdTerminal {_pName = "NAME", _bindVar = Just ("n",BindAssign)},ProdTerminal {_pName = "FALSE", _bindVar = Nothing},ProdTerminal {_pName = "TRUE", _bindVar = Nothing},ProdTerminal {_pName = "STRLIT", _bindVar = Nothing},ProdTerminal {_pName = "CHARLIT", _bindVar = Nothing},ProdTerminal {_pName = "INTLIT", _bindVar = Nothing}]),("pureType",[ProdTerminal {_pName = "PARENSQL", _bindVar = Nothing},ProdTerminal {_pName = "PURETYPE", _bindVar = Nothing}]),("retterm",[ProdTerminal {_pName = "NAME", _bindVar = Just ("n",BindAssign)},ProdTerminal {_pName = "PARENSQL", _bindVar = Nothing},ProdTerminal {_pName = "FALSE", _bindVar = Nothing},ProdTerminal {_pName = "TRUE", _bindVar = Nothing},ProdTerminal {_pName = "STRLIT", _bindVar = Nothing},ProdTerminal {_pName = "CHARLIT", _bindVar = Nothing},ProdTerminal {_pName = "INTLIT", _bindVar = Nothing},ProdTerminal {_pName = "LET", _bindVar = Nothing},ProdTerminal {_pName = "PARENL", _bindVar = Nothing},ProdTerminal {_pName = "IF", _bindVar = Nothing}]),("term",[ProdTerminal {_pName = "NAME", _bindVar = Just ("n",BindAssign)},ProdTerminal {_pName = "PARENSQL", _bindVar = Nothing},ProdTerminal {_pName = "FALSE", _bindVar = Nothing},ProdTerminal {_pName = "TRUE", _bindVar = Nothing},ProdTerminal {_pName = "STRLIT", _bindVar = Nothing},ProdTerminal {_pName = "CHARLIT", _bindVar = Nothing},ProdTerminal {_pName = "INTLIT", _bindVar = Nothing},ProdTerminal {_pName = "LET", _bindVar = Nothing},ProdTerminal {_pName = "PARENL", _bindVar = Nothing},ProdTerminal {_pName = "IF", _bindVar = Nothing}]),("topLevel",[ProdTerminal {_pName = "NL", _bindVar = Nothing},ProdEpsilon]),("type",[ProdTerminal {_pName = "PARENSQL", _bindVar = Nothing},ProdTerminal {_pName = "PURETYPE", _bindVar = Nothing},ProdTerminal {_pName = "PARENL", _bindVar = Nothing}])]

{-
_gen0 -> "PARENSQR", "PARENSQL", "NAME", "FALSE", "TRUE", "STRLIT", "CHARLIT", "INTLIT", "NL", "LET", "PARENL", "IF", "IN", "PARENR", "INFIX", "EQUALS", "NAME", "ELSE", "THEN", "EOF", "$", "COMMA"
_gen10 -> ""
_gen11 -> "NL", "PARENSQL", "NAME", "FALSE", "TRUE", "STRLIT", "CHARLIT", "INTLIT"
_gen2 -> "NAME"
_gen4 -> "NL"
_gen6 -> "NAME", "HOLE"
_gen8 -> "PARENSQL", "NAME", "FALSE", "TRUE", "STRLIT", "CHARLIT", "INTLIT"
_genComb1 -> "COMMA", "PARENSQL", "NAME", "FALSE", "TRUE", "STRLIT", "CHARLIT", "INTLIT", "PARENSQR", "NL", "LET", "PARENL", "IF", "IN", "PARENR", "INFIX", "EQUALS", "NAME", "ELSE", "THEN", "EOF", "$"
_genComb12 -> "NL", "PARENSQL", "NAME", "FALSE", "TRUE", "STRLIT", "CHARLIT", "INTLIT"
_genComb3 -> "NAME"
_genComb5 -> ""
_genComb7 -> ""
_genComb9 -> ""
args -> "EQUALS", "VERTBAR"
decl -> "NL"
func -> "NL", "PARENR", "INFIX", "EQUALS", "$"
primValue -> "NL", "PARENSQL", "NAME", "FALSE", "TRUE", "STRLIT", "CHARLIT", "INTLIT"
pureType -> "PARENSQR", "ARROW"
retterm -> "NL", "EQUALS", "PARENR", "INFIX"
term -> "PARENR", "INFIX", "NL", "EQUALS"
topLevel -> "$"
type -> "PARENR", "NL"
-}

followGen :: Map Text [Maybe ProdItem]
followGen = M.fromList [("_gen0",[Just (ProdTerminal {_pName = "PARENSQR", _bindVar = Nothing}),Just (ProdTerminal {_pName = "PARENSQL", _bindVar = Nothing}),Just (ProdTerminal {_pName = "NAME", _bindVar = Just ("n",BindAssign)}),Just (ProdTerminal {_pName = "FALSE", _bindVar = Nothing}),Just (ProdTerminal {_pName = "TRUE", _bindVar = Nothing}),Just (ProdTerminal {_pName = "STRLIT", _bindVar = Nothing}),Just (ProdTerminal {_pName = "CHARLIT", _bindVar = Nothing}),Just (ProdTerminal {_pName = "INTLIT", _bindVar = Nothing}),Just (ProdTerminal {_pName = "NL", _bindVar = Nothing}),Just (ProdTerminal {_pName = "LET", _bindVar = Nothing}),Just (ProdTerminal {_pName = "PARENL", _bindVar = Nothing}),Just (ProdTerminal {_pName = "IF", _bindVar = Nothing}),Just (ProdTerminal {_pName = "IN", _bindVar = Nothing}),Just (ProdTerminal {_pName = "PARENR", _bindVar = Nothing}),Just (ProdTerminal {_pName = "INFIX", _bindVar = Nothing}),Just (ProdTerminal {_pName = "EQUALS", _bindVar = Nothing}),Just (ProdTerminal {_pName = "NAME", _bindVar = Nothing}),Just (ProdTerminal {_pName = "ELSE", _bindVar = Nothing}),Just (ProdTerminal {_pName = "THEN", _bindVar = Nothing}),Just (ProdTerminal {_pName = "EOF", _bindVar = Nothing}),Nothing,Just (ProdTerminal {_pName = "COMMA", _bindVar = Nothing})]),("_gen10",[]),("_gen11",[Just (ProdTerminal {_pName = "NL", _bindVar = Nothing}),Just (ProdTerminal {_pName = "PARENSQL", _bindVar = Nothing}),Just (ProdTerminal {_pName = "NAME", _bindVar = Just ("n",BindAssign)}),Just (ProdTerminal {_pName = "FALSE", _bindVar = Nothing}),Just (ProdTerminal {_pName = "TRUE", _bindVar = Nothing}),Just (ProdTerminal {_pName = "STRLIT", _bindVar = Nothing}),Just (ProdTerminal {_pName = "CHARLIT", _bindVar = Nothing}),Just (ProdTerminal {_pName = "INTLIT", _bindVar = Nothing})]),("_gen2",[Just (ProdTerminal {_pName = "NAME", _bindVar = Nothing})]),("_gen4",[Just (ProdTerminal {_pName = "NL", _bindVar = Nothing})]),("_gen6",[Just (ProdTerminal {_pName = "NAME", _bindVar = Just ("a",BindAdd)}),Just (ProdTerminal {_pName = "HOLE", _bindVar = Just ("a",BindAdd)})]),("_gen8",[Just (ProdTerminal {_pName = "PARENSQL", _bindVar = Nothing}),Just (ProdTerminal {_pName = "NAME", _bindVar = Just ("n",BindAssign)}),Just (ProdTerminal {_pName = "FALSE", _bindVar = Nothing}),Just (ProdTerminal {_pName = "TRUE", _bindVar = Nothing}),Just (ProdTerminal {_pName = "STRLIT", _bindVar = Nothing}),Just (ProdTerminal {_pName = "CHARLIT", _bindVar = Nothing}),Just (ProdTerminal {_pName = "INTLIT", _bindVar = Nothing})]),("_genComb1",[Just (ProdTerminal {_pName = "COMMA", _bindVar = Nothing}),Just (ProdTerminal {_pName = "PARENSQL", _bindVar = Nothing}),Just (ProdTerminal {_pName = "NAME", _bindVar = Just ("n",BindAssign)}),Just (ProdTerminal {_pName = "FALSE", _bindVar = Nothing}),Just (ProdTerminal {_pName = "TRUE", _bindVar = Nothing}),Just (ProdTerminal {_pName = "STRLIT", _bindVar = Nothing}),Just (ProdTerminal {_pName = "CHARLIT", _bindVar = Nothing}),Just (ProdTerminal {_pName = "INTLIT", _bindVar = Nothing}),Just (ProdTerminal {_pName = "PARENSQR", _bindVar = Nothing}),Just (ProdTerminal {_pName = "NL", _bindVar = Nothing}),Just (ProdTerminal {_pName = "LET", _bindVar = Nothing}),Just (ProdTerminal {_pName = "PARENL", _bindVar = Nothing}),Just (ProdTerminal {_pName = "IF", _bindVar = Nothing}),Just (ProdTerminal {_pName = "IN", _bindVar = Nothing}),Just (ProdTerminal {_pName = "PARENR", _bindVar = Nothing}),Just (ProdTerminal {_pName = "INFIX", _bindVar = Nothing}),Just (ProdTerminal {_pName = "EQUALS", _bindVar = Nothing}),Just (ProdTerminal {_pName = "NAME", _bindVar = Nothing}),Just (ProdTerminal {_pName = "ELSE", _bindVar = Nothing}),Just (ProdTerminal {_pName = "THEN", _bindVar = Nothing}),Just (ProdTerminal {_pName = "EOF", _bindVar = Nothing}),Nothing]),("_genComb12",[Just (ProdTerminal {_pName = "NL", _bindVar = Nothing}),Just (ProdTerminal {_pName = "PARENSQL", _bindVar = Nothing}),Just (ProdTerminal {_pName = "NAME", _bindVar = Just ("n",BindAssign)}),Just (ProdTerminal {_pName = "FALSE", _bindVar = Nothing}),Just (ProdTerminal {_pName = "TRUE", _bindVar = Nothing}),Just (ProdTerminal {_pName = "STRLIT", _bindVar = Nothing}),Just (ProdTerminal {_pName = "CHARLIT", _bindVar = Nothing}),Just (ProdTerminal {_pName = "INTLIT", _bindVar = Nothing})]),("_genComb3",[Just (ProdTerminal {_pName = "NAME", _bindVar = Nothing})]),("_genComb5",[]),("_genComb7",[]),("_genComb9",[]),("args",[Just (ProdTerminal {_pName = "EQUALS", _bindVar = Nothing}),Just (ProdTerminal {_pName = "VERTBAR", _bindVar = Nothing})]),("decl",[Just (ProdTerminal {_pName = "NL", _bindVar = Nothing})]),("func",[Just (ProdTerminal {_pName = "NL", _bindVar = Nothing}),Just (ProdTerminal {_pName = "PARENR", _bindVar = Nothing}),Just (ProdTerminal {_pName = "INFIX", _bindVar = Nothing}),Just (ProdTerminal {_pName = "EQUALS", _bindVar = Nothing}),Nothing]),("primValue",[Just (ProdTerminal {_pName = "NL", _bindVar = Nothing}),Just (ProdTerminal {_pName = "PARENSQL", _bindVar = Nothing}),Just (ProdTerminal {_pName = "NAME", _bindVar = Just ("n",BindAssign)}),Just (ProdTerminal {_pName = "FALSE", _bindVar = Nothing}),Just (ProdTerminal {_pName = "TRUE", _bindVar = Nothing}),Just (ProdTerminal {_pName = "STRLIT", _bindVar = Nothing}),Just (ProdTerminal {_pName = "CHARLIT", _bindVar = Nothing}),Just (ProdTerminal {_pName = "INTLIT", _bindVar = Nothing})]),("pureType",[Just (ProdTerminal {_pName = "PARENSQR", _bindVar = Nothing}),Just (ProdTerminal {_pName = "ARROW", _bindVar = Nothing})]),("retterm",[Just (ProdTerminal {_pName = "NL", _bindVar = Nothing}),Just (ProdTerminal {_pName = "EQUALS", _bindVar = Nothing}),Just (ProdTerminal {_pName = "PARENR", _bindVar = Nothing}),Just (ProdTerminal {_pName = "INFIX", _bindVar = Nothing})]),("term",[Just (ProdTerminal {_pName = "PARENR", _bindVar = Nothing}),Just (ProdTerminal {_pName = "INFIX", _bindVar = Nothing}),Just (ProdTerminal {_pName = "NL", _bindVar = Nothing}),Just (ProdTerminal {_pName = "EQUALS", _bindVar = Nothing})]),("topLevel",[Nothing]),("type",[Just (ProdTerminal {_pName = "PARENR", _bindVar = Nothing}),Just (ProdTerminal {_pName = "NL", _bindVar = Nothing})])]

----------------------------------------------------------------------------
-- Tokens
----------------------------------------------------------------------------

tokensGen :: [TokenExp]
tokensGen = [TokenExp {tName = "WHITESPACE", tRegex = "[ \\t]+", tSkip = True},TokenExp {tName = "HOLE", tRegex = "_", tSkip = False},TokenExp {tName = "TRUE", tRegex = "True", tSkip = False},TokenExp {tName = "FALSE", tRegex = "False", tSkip = False},TokenExp {tName = "ARROW", tRegex = "->|\8594", tSkip = False},TokenExp {tName = "DOUBLECOLON", tRegex = "::", tSkip = False},TokenExp {tName = "PURETYPE", tRegex = "(Int|Bool|Char)", tSkip = False},TokenExp {tName = "VERTBAR", tRegex = "\\|", tSkip = False},TokenExp {tName = "EQUALS", tRegex = "=", tSkip = False},TokenExp {tName = "COMMA", tRegex = ",", tSkip = False},TokenExp {tName = "LET", tRegex = "let", tSkip = False},TokenExp {tName = "IF", tRegex = "if", tSkip = False},TokenExp {tName = "THEN", tRegex = "then", tSkip = False},TokenExp {tName = "ELSE", tRegex = "else", tSkip = False},TokenExp {tName = "PARENL", tRegex = "\\(", tSkip = False},TokenExp {tName = "PARENR", tRegex = "\\)", tSkip = False},TokenExp {tName = "PARENSQL", tRegex = "\\[", tSkip = False},TokenExp {tName = "PARENSQR", tRegex = "\\]", tSkip = False},TokenExp {tName = "INTLIT", tRegex = "[+-]?[0-9]+", tSkip = False},TokenExp {tName = "INFIX", tRegex = "([\\+\\-\\*\\<\\>\\^\\/] | '==' | '/=' | '<=' | '>=')", tSkip = False},TokenExp {tName = "NAME", tRegex = "[a-z][a-zA-Z\\'_0-9]*", tSkip = False},TokenExp {tName = "CHARLIT", tRegex = "[\\'][a-zA-Z0-9]?[\\']", tSkip = False},TokenExp {tName = "STRLIT", tRegex = "[\\\"][a-zA-Z0-9]*[\\\"]", tSkip = False},TokenExp {tName = "NL", tRegex = "[\\r\\n]+", tSkip = False}]
