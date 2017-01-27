{-# LANGUAGE TemplateHaskell #-}
-- | Actions on grammar

module Haskll.Grammar (convertGrammar) where

import           Control.Lens             (makeLenses, uses, (%=), (<<+=), (<>=))
import qualified Data.Map                 as M
import qualified Data.Text.IO             as TIO
import           Universum

import           Haskll.Syntax.Expression (Expression (..), GrammarDef (..), Term (..))
import           Haskll.Syntax.Parser
import           Haskll.Types             (BindType (..), GrammarRule (..), ProdItem (..),
                                           bindVar, prettyGrammarRule)


data Combinator = CMany | CSome | COpt deriving (Show,Eq,Ord)

combApply :: Combinator -> ProdItem -> ProdItem -> [[ProdItem]]
combApply CMany t' genT = [[ProdEpsilon], [t', genT]]
combApply CSome t' genT = [[t', genT]]
combApply COpt t' _     = [[ProdEpsilon], [t']]

data GState = GState
    { _rulesReuse     :: Map Term Text
    , _combRulesReuse :: Map (Term, Combinator) Text
    , _subGrams       :: [GrammarRule]
    , _genNumber      :: Int
    } deriving (Show)

makeLenses ''GState

newtype GrammarT a = GrammarT
    { toGrammarT :: State GState a
    } deriving (Functor, Applicative, Monad, MonadState GState)

nontermEmpty :: Text -> ProdItem
nontermEmpty n = ProdNonterminal n Nothing Nothing

genRule :: Term -> GrammarT Text
genRule t = do
    exists <- uses rulesReuse $ M.lookup t
    maybe genNew pure exists
  where
    genNew = do
        num <- genNumber <<+= 1
        let genName = "_gen" <> show num
        expr <- fromExpression $ Expression genName [] [] [] t
        rulesReuse %= M.insert t genName
        subGrams <>= expr
        pure genName

nonTermGenCombinator :: Term
                     -> Combinator
                     -> GrammarT ProdItem
nonTermGenCombinator t comb =
    maybe onGen onReuse =<< uses combRulesReuse (M.lookup (t,comb))
  where
    onReuse = pure . nontermEmpty
    onGen = do
        t' <- nontermEmpty <$> genRule t
        num <- genNumber <<+= 1
        let genName = "_genComb" <> show num
            prodItemsCases = combApply comb t' (nontermEmpty genName)
            gRule = map (\items -> GrammarRule genName items [] [] []) prodItemsCases
        subGrams <>= gRule
        combRulesReuse %= M.insert (t,comb) genName
        pure $ nontermEmpty genName

collectItems :: Term -> GrammarT [ProdItem]
collectItems (t1 :&: t2)       = (++) <$> collectItems t1 <*> collectItems t2
collectItems (WithCode t code) = (++ [ProdCode code]) <$> collectItems t
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
collectItem ((:*:) t)         = nonTermGenCombinator t CMany
collectItem ((:+:) t)         = nonTermGenCombinator t CSome
collectItem ((:?:) t)         = nonTermGenCombinator t COpt

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
    (outputed,stateAfter) = runState (toGrammarT topLvl) (GState M.empty M.empty [] 0)
    topLvl = concat <$> mapM fromExpression es

kek = do
    (Right g) <- parseGrammar <$> TIO.readFile "resources/test3.g"
    forM_ (convertGrammar $ gExprs g) $ putStrLn . prettyGrammarRule
