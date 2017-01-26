{-# LANGUAGE TemplateHaskell #-}
-- | Actions on grammar

module Haskll.Grammar where

import           Control.Lens             (makeLenses, (<<+=), (<>=))
import qualified Data.Text.IO             as TIO
import           Universum

import           Haskll.Syntax.Expression (Expression (..), GrammarDef (..), Term (..))
import           Haskll.Syntax.Parser
import           Haskll.Types             (BindType (..), GrammarRule (..), ProdItem (..),
                                           bindVar)

data GState = GState
    { _subGrams  :: [GrammarRule]
    , _genNumber :: Int
    } deriving (Show)

makeLenses ''GState

newtype GrammarT a = GrammarT
    { toGrammarT :: State GState a
    } deriving (Functor, Applicative, Monad, MonadState GState)

genRule :: Term -> GrammarT Text
genRule t = do
    num <- genNumber <<+= 1
    let genName = "_generated" <> show num
    expr <- fromExpression $ Expression genName [] [] [] t
    subGrams <>= expr
    pure genName

nonTermGenCombinator :: Term
                     -> (ProdItem -> ProdItem -> [[ProdItem]])
                     -> GrammarT ProdItem
nonTermGenCombinator t combine = do
    t' <- collectItem t
    num <- genNumber <<+= 1
    let genName = "_generatedComb" <> show num
        prodItemsCases = combine t' (ProdNonterminal genName Nothing Nothing)
        gRule = map (\items -> GrammarRule genName items [] [] []) prodItemsCases
    subGrams <>= gRule
    pure $ ProdNonterminal genName Nothing Nothing

collectItems :: Term -> GrammarT [ProdItem]
collectItems (t1 :&: t2)       = (++) <$> collectItems t1 <*> collectItems t2
collectItems (WithCode t code) = (ProdCode code :) <$> collectItems t
collectItems (Subterm t)       = collectItems t
collectItems t                 = one <$> collectItem t

nonTermGen :: Term -> GrammarT ProdItem
nonTermGen t = do
    generatedName <- genRule t
    pure $ ProdNonterminal generatedName Nothing Nothing

collectItem :: Term -> GrammarT ProdItem
collectItem (Subterm t)       = collectItem t
collectItem (TermToken tName) = pure $ ProdTerminal tName Nothing
collectItem (TermOther t mc)  = pure $ ProdNonterminal t mc Nothing
collectItem (_ :&: _)         = panic "collectItem: encountered :&:"
collectItem (v :+=: t)        = collectItem t <&> bindVar .~ Just (v, BindAdd)
collectItem (v ::=: t)        = collectItem t <&> bindVar .~ Just (v, BindAssign)
collectItem t@(_ :|: _)       = nonTermGen t
collectItem t@(WithCode _ _)  = nonTermGen t
collectItem ((:*:) t)         = nonTermGenCombinator t (\t' genT -> [[ProdEpsilon], [genT, t']])
collectItem ((:+:) t)         = nonTermGenCombinator t (\t' genT -> [[genT, t']])
collectItem ((:?:) t)         = nonTermGenCombinator t (\t' _    -> [[ProdEpsilon], [t']])

fromExpression :: Expression -> GrammarT [GrammarRule]
fromExpression Expression {..} = topOr eTerm
  where
    gName = eName
    gReceivingAttrs = eReceivingAttrs
    gGeneratingAttrs = eGeneratingAttrs
    gLocals = eLocals
    topOr (t1 :|: t2) = (++) <$> topOr t1 <*> topOr t2
    topOr (Subterm t) = topOr t
    topOr t           = (:[]) <$> topExp t
    topExp t1 = do
        gProd <- collectItems t1
        pure $ GrammarRule {.. }

convertGrammar :: [Expression] -> [GrammarRule]
convertGrammar es = outputed ++ stateAfter ^. subGrams
  where
    (outputed,stateAfter) = runState (toGrammarT topLvl) (GState [] 0)
    topLvl = concat <$> mapM fromExpression es

kek  = do
    t <- TIO.readFile "resources/test2.g"
    f <- TIO.readFile "resources/simple.sf"
    let (Right g) = parseGrammar t
    print $ convertGrammar $ gExprs g
    undefined
