{-# LANGUAGE TemplateHaskell #-}
-- | Actions on grammar

module Haskll.Grammar where

import           Control.Lens             (makeLenses, (<<+=), (<>=))
import qualified Data.Text.IO             as TIO
import           Universum

import           Haskll.Syntax.Expression (Expression (..), GrammarDef (..), Term (..))
import           Haskll.Syntax.Parser
import           Haskll.Types             (BindType (..), GrammarRule (..), ProdItem (..),
                                           bindVar, prettyGrammarRule)

data GState = GState
    { _subGrams  :: [GrammarRule]
    , _genNumber :: Int
    } deriving (Show)

makeLenses ''GState

newtype GrammarT a = GrammarT
    { toGrammarT :: State GState a
    } deriving (Functor, Applicative, Monad, MonadState GState)

nontermEmpty :: Text -> ProdItem
nontermEmpty n = ProdNonterminal n Nothing Nothing

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
    t' <- nontermEmpty <$> genRule t
    num <- genNumber <<+= 1
    let genName = "_generatedComb" <> show num
        prodItemsCases = combine t' (nontermEmpty genName)
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
collectItem t@(_ :&: _)       = panic $ "collectItem: encountered :&: " <> show t
collectItem (v :+=: t)        = collectItem t <&> bindVar .~ Just (v, BindAdd)
collectItem (v ::=: t)        = collectItem t <&> bindVar .~ Just (v, BindAssign)
collectItem t@(_ :|: _)       = nonTermGen t
collectItem t@(WithCode _ _)  = nonTermGen t
collectItem ((:*:) t)         = nonTermGenCombinator t (\t' genT -> [[ProdEpsilon], [t', genT]])
collectItem ((:+:) t)         = nonTermGenCombinator t (\t' genT -> [[t', genT]])
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
    (Right g) <- parseGrammar <$> TIO.readFile "resources/test2.g"
    forM_ (convertGrammar $ gExprs g) $ putStrLn . prettyGrammarRule
    undefined
