{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
-- | Code generation

module Haskll.Codegen () where

import           Control.Lens             (at, makeLenses, (%=))
import           Data.List                (nub)
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
                                           bindVar, pName, prettyGrammarRule)

data FirstState = FirstState { _fsMap :: Map Text [ProdItem] }

makeLenses ''FirstState

setFirst :: [GrammarRule] -> Map Text [ProdItem]
setFirst rules = _fsMap $ execState loop initState
  where
    initState = FirstState $ M.fromList $ map (\x -> (gName x,[])) rules
    loop :: (MonadState FirstState m) => m ()
    loop = do
        init <- use fsMap
        forM_ rules $ \GrammarRule{..} -> case unsafeHead gProd of
            p@ProdTerminal {..} -> fsMap . at gName %= fmap (nub . (p:))
            p@ProdNonterminal {..} -> do
                (curfirst :: Map Text [ProdItem]) <- use fsMap
                fsMap . at gName %= fmap (nub . ((curfirst ! (p ^. pName))++))
            _ -> pass
        after <- use fsMap
        when (init /= after) loop

kek  = do
    (Right g) <- parseGrammar <$> TIO.readFile "resources/test2.g"
    let expressions = convertGrammar $ gExprs g
    forM_ (M.assocs $ setFirst expressions) $ \(text,items) ->
        putStrLn $ text <> " -> \"" <> T.intercalate "\", \"" (map (view pName) items) <> "\""
