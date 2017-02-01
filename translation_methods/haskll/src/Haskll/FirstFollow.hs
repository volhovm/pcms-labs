{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE ViewPatterns     #-}

-- | First/Follow sets generation

module Haskll.FirstFollow
       ( FirstSet
       , FollowSet
       , setFirst
       , setFollow
       , setFirstFollow
       , testFirstFollow
       , checkLL1
       ) where

import           Control.Lens             (ASetter, at, makeLenses, (%=))
import           Data.List                (delete, intersect, nub, (!!))
import qualified Data.Map                 as M
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Universum
import           Unsafe                   (unsafeHead)

import           Haskll.Grammar           (convertGrammar)
import           Haskll.Syntax.Expression (gExprs)
import           Haskll.Syntax.Parser     (parseGrammar)
import           Haskll.Types             (GrammarRule (..), ProdItem (..),
                                           filterProdCode, grProds, notCode, pName,
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

(!) :: (Ord k,Show k) => Map k v -> k -> v
(!) m k = fromMaybe (panic $ "Key " <> show k <> " is not in map") $ M.lookup k m

forFiltered :: (Ord a, Eq b, MonadState (MapWrap a [b]) m) =>
              (a -> Bool) -> ([b] -> [b]) -> m ()
forFiltered predicate action = do
    inner <- use mapw
    forM_ (filter predicate $ M.keys inner) $ \k ->
        mapw . at k %= fmap (nub . action)

setFirst :: [GrammarRule] -> FirstSet
setFirst (map filterProdCode -> rules) = _mapw $ execState loop initState
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
setFirstFollow (map filterProdCode -> rules) =
    (sfirst, _mapw $ execState loop initState)
  where
    sfirst = setFirst rules
    splitItems :: [a] -> [([a],a,[a])]
    splitItems items =
        map (\i -> (take i items, items !! i, drop (i+1) items)) [0..length items - 1]
    initState = MapWrap $ M.fromList $
        ("start", [Nothing]) : map (\g -> (grName g,[])) (drop 1 rules)
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
                        let followA = curfollow ! (after ^. pName)
                        mapw . at (focus ^. pName) <>%= (followA ++)
                splitFoo (_,focus,t@ProdTerminal{..}:_) = do
                    mapw . at (focus ^. pName) <>%= (Just t :)
                splitFoo (b,focus,ProdEpsilon:xs) = splitFoo (b,focus,xs)
                splitFoo (b,focus,(ProdCode _):xs) = splitFoo (b,focus,xs)
                splitFoo (_,focus,[]) = do
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
    putText "Parsed"
    let expressions = convertGrammar $ gExprs g
    forM_ (M.assocs $ setFirst expressions) $ \(text,items) ->
        putStrLn $ text <> " -> \"" <> T.intercalate "\", \"" (map prettyProdItem items) <> "\""
    putText "---------"
    forM_ (M.assocs $ setFollow expressions) $ \(gr,items) -> do
        let realItems = map (maybe "$" prettyProdItem) items
        putStrLn $ gr <> " -> \"" <> T.intercalate "\", \"" realItems <> "\""

-- Returns broken rule A if sees that first(a) = first(b), where A ->
-- ax, A -> by.
checkLL1 :: [GrammarRule] -> [(GrammarRule, [ProdItem], [ProdItem])]
checkLL1 rules = concat rulesChecked
  where
    firstS = setFirst rules
    pairs []     = []
    pairs (x:xs) = map (x,) xs ++ pairs xs
    withPName = filter (not . T.null) . map (view pName)
    getFirst :: ProdItem -> [Text]
    getFirst (ProdNonterminal pname _ _) = withPName $ firstS ! pname
    getFirst (ProdTerminal pname _)      = [pname]
    getFirst _                           = []
    noCode = filter notCode
    prodsCompatible :: [ProdItem] -> [ProdItem] -> Bool
    prodsCompatible (noCode -> (a:_)) (noCode -> (b:_)) =
        null $ getFirst a `intersect` getFirst b
    prodsCompatible _ _ = panic "prodsCompatible"
    checkRule :: GrammarRule -> [[ProdItem]] -> [(GrammarRule, [ProdItem], [ProdItem])]
    checkRule g prods =
        map (\(a,b) -> (g,a,b)) $
        filter (not . uncurry prodsCompatible) (pairs prods)
    rulesChecked = map (\g@GrammarRule{..} -> checkRule g grProds) rules
