{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
-- | First/Follow sets generation

module Haskll.FirstFollow
       ( FirstSet
       , FollowSet
       , setFirst
       , setFollow
       , setFirstFollow
       , testFirstFollow
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
import           Haskll.Types             (GrammarRule (..), ProdItem (..), pName,
                                           prettyProdItem)

type FirstSet = Map Text [ProdItem]
type FollowSet = Map Text [Maybe ProdItem]

data MapWrap a b = MapWrap { _mapw :: Map a b }
makeLenses ''MapWrap

(<>%=)
    :: (MonadState s m, Functor f, Eq b)
    => ASetter s s (f a) (f [b]) -> (a -> [b]) -> m ()
(<>%=) a b = a %= fmap (nub . b)
infixr 8 <>%=

forFiltered :: (Ord a, Eq b, MonadState (MapWrap a [b]) m) =>
              (a -> Bool) -> ([b] -> [b]) -> m ()
forFiltered predicate action = do
    inner <- use mapw
    forM_ (filter predicate $ M.keys inner) $ \k ->
        mapw . at k %= fmap (nub . action)

setFirst :: [GrammarRule] -> FirstSet
setFirst rules = _mapw $ execState loop initState
  where
    initState = MapWrap $ M.fromList $ map (\x -> (grName x,[])) rules
    loop :: (MonadState (MapWrap Text [ProdItem]) m) => m ()
    loop = do
        init <- use mapw
        forM_ rules $ \GrammarRule{..} ->
            forM_ grProds $ \grProd -> case unsafeHead grProd of
                ProdCode _  -> pass
                p@ProdNonterminal {..} -> do
                    curfirst <- use mapw
                    mapw . at grName <>%= ((curfirst ! (p ^. pName))++)
                p -> mapw . at grName <>%= (p:)
        after <- use mapw
        when (init /= after) loop

-- head should be starting terminal
setFirstFollow :: [GrammarRule] -> (FirstSet, FollowSet)
setFirstFollow rules = (sfirst, _mapw $ execState loop initState)
  where
    sfirst = setFirst rules
    splitItems :: [a] -> [([a],a,[a])]
    splitItems items =
        map (\i -> (take i items, items !! i, drop (i+1) items)) [0..length items - 1]
    initState = MapWrap $ M.fromList $
        (grName $ unsafeHead rules, [Nothing]) : map (\g -> (grName g,[])) (drop 1 rules)
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
                        let followA = curfollow ! grName gCurrent
                        mapw . at (focus ^. pName) <>%= (followA ++)
                splitFoo (_,focus,t@ProdTerminal{..}:_) = do
                    mapw . at (focus ^. pName) <>%= (Just t :)
                splitFoo (_,focus,ProdEpsilon:_) = do
                    curfollow <- use mapw
                    mapw . at (focus ^. pName) <>%= ((curfollow ! grName gCurrent)++)
                splitFoo (_,focus,[]) = do -- duh, copypaste
                    curfollow <- use mapw
                    mapw . at (focus ^. pName) <>%= ((curfollow ! grName gCurrent)++)
                splitFoo _ = pass
            forM_ (grProds gCurrent) $ \grProd -> forM_ (splitItems grProd) splitFoo
        after <- use mapw
        when (init /= after) loop

setFollow :: [GrammarRule] -> FollowSet
setFollow = snd . setFirstFollow

testFirstFollow :: IO ()
testFirstFollow  = do
    (Right g) <- parseGrammar <$> TIO.readFile "resources/test2.g"
    let expressions = convertGrammar $ gExprs g
    forM_ (M.assocs $ setFirst expressions) $ \(text,items) ->
        putStrLn $ text <> " -> \"" <> T.intercalate "\", \"" (map prettyProdItem items) <> "\""
    putText "---------"
    forM_ (M.assocs $ setFollow expressions) $ \(gr,items) -> do
        let realItems = map (maybe "$" prettyProdItem) items
        putStrLn $ gr <> " -> \"" <> T.intercalate "\", \"" realItems <> "\""
