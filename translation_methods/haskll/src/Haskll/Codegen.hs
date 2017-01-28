{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
-- | Code generation

module Haskll.Codegen
       ( setFirst
       , setFollow
       ) where

import           Control.Lens             (ASetter, at, makeLenses, (%=))
import           Data.List                (delete, nub, (!!))
import           Data.Map                 ((!))
import qualified Data.Map                 as M
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Universum
import           Unsafe                   (unsafeHead)

import           Haskll.Grammar           (convertGrammar)
import           Haskll.Syntax.Expression (gExprs)
import           Haskll.Syntax.Parser     (parseGrammar)
import           Haskll.Types             (BindType (..), GrammarRule (..), ProdItem (..),
                                           bindVar, pName, prettyGrammarRule,
                                           prettyProdItem)

----------------------------------------------------------------------------
-- FIRST / FOLLOW
----------------------------------------------------------------------------

data MapWrap a b = MapWrap { _mapw :: Map a b }
makeLenses ''MapWrap

(<>%=)
    :: (MonadState s m, Functor f, Eq b)
    => ASetter s s (f a) (f [b]) -> (a -> [b]) -> m ()
(<>%=) a b = a %= fmap (nub . b)
infixr 8 <>%=

forFiltered :: (Ord a, MonadState (MapWrap a b) m) =>
              (a -> Bool) -> (b -> b) -> m ()
forFiltered predicate action = do
    inner <- use mapw
    forM_ (filter predicate $ M.keys inner) $ \k ->
        mapw . at k %= fmap action

setFirst :: [GrammarRule] -> Map Text [ProdItem]
setFirst rules = _mapw $ execState loop initState
  where
    initState = MapWrap $ M.fromList $ map (\x -> (gName x,[])) rules
    loop :: (MonadState (MapWrap Text [ProdItem]) m) => m ()
    loop = do
        init <- use mapw
        forM_ rules $ \GrammarRule{..} -> case unsafeHead gProd of
            ProdCode _  -> pass
            p@ProdNonterminal {..} -> do
                curfirst <- use mapw
                mapw . at gName <>%= ((curfirst ! (p ^. pName))++)
            p -> mapw . at gName <>%= (p:)
        after <- use mapw
        when (init /= after) loop

-- head should be starting terminal
setFollow :: [GrammarRule] -> Map Text [Maybe ProdItem]
setFollow rules = _mapw $ execState loop initState
  where
    sfirst = setFirst rules
    splitItems :: [a] -> [([a],a,[a])]
    splitItems items =
        map (\i -> (take i items, items !! i, drop (i+1) items)) [0..length items - 1]
    initState = MapWrap $ M.fromList $
        (gName $ unsafeHead rules, [Nothing]) : map (\g -> (gName g,[])) (drop 1 rules)
    loop :: (MonadState (MapWrap Text [Maybe ProdItem]) m) => m ()
    loop = do
        init <- use mapw
        forM_ rules $ \gCurrent -> do
            let splitFoo (_,focus,after@ProdNonterminal{..}:_) = do
                    let tmp1 = sfirst ! view pName after
                    forFiltered (== view pName focus)
                        (map Just (delete ProdEpsilon tmp1) ++)
                    when (ProdEpsilon `elem` tmp1) $ do
                        curfollow <- use mapw
                        let followA = curfollow ! gName gCurrent
                        mapw . at (focus ^. pName) <>%= (followA ++)
                splitFoo (_,focus,t@ProdTerminal{..}:_) = do
                    mapw . at (focus ^. pName) <>%= (Just t :)
                splitFoo (_,focus,ProdEpsilon:_) = do
                    curfollow <- use mapw
                    mapw . at (focus ^. pName) <>%= ((curfollow ! gName gCurrent)++)
                splitFoo (_,focus,[]) = do -- duh, copypaste
                    curfollow <- use mapw
                    mapw . at (focus ^. pName) <>%= ((curfollow ! gName gCurrent)++)
                splitFoo _ = pass
            forM_ (splitItems $ gProd gCurrent) splitFoo
        after <- use mapw
        when (init /= after) loop

kek :: IO ()
kek  = do
    (Right g) <- parseGrammar <$> TIO.readFile "resources/test3.g"
    let expressions = convertGrammar $ gExprs g
    forM_ (M.assocs $ setFirst expressions) $ \(text,items) ->
        putStrLn $ text <> " -> \"" <> T.intercalate "\", \"" (map prettyProdItem items) <> "\""
    putText "---------"
    forM_ (M.assocs $ setFollow expressions) $ \(gr,items) -> do
        let realItems = map (maybe "$" prettyProdItem) items
        putStrLn $ gr <> " -> \"" <> T.intercalate "\", \"" realItems <> "\""

----------------------------------------------------------------------------
-- Code generation
----------------------------------------------------------------------------
