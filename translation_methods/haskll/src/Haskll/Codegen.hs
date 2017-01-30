{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
-- | Code generation

module Haskll.Codegen
       ( genParser
       ) where

import           Control.Lens             (ASetter, at, makeLenses, (%=))
import           Data.FileEmbed           (embedStringFile, makeRelativeToProject)
import           Data.List                (delete, nub, (!!))
import           Data.Map                 ((!))
import qualified Data.Map                 as M
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Universum
import           Unsafe                   (unsafeHead)

import           Haskll.FirstFollow       (setFirstFollow)
import           Haskll.Grammar           (convertGrammar)
import           Haskll.Syntax.Expression (GrammarDef (..))
import           Haskll.Syntax.Parser     (parseGrammar)
import           Haskll.Types             (BindType (..), GrammarRule (..), ProdItem (..),
                                           bindVar, pName, prettyGrammarRule,
                                           prettyProdItem)

----------------------------------------------------------------------------
-- Code generation
----------------------------------------------------------------------------

codegenBase :: Text
codegenBase = $(embedStringFile =<< makeRelativeToProject "./src/Haskll/CodegenBase.hs")

type TextT a = State Text a

appNL :: TextT ()
appNL = appLine ""

appText, appLine, appComment, appFullComment :: Text -> TextT ()
appText t = identity %= (<> t)
appLine t = appText t >> appText "\n"
appComment t = do
    let lline = T.pack (replicate 76 '-')
    appNL >> appLine lline
    appLine $ "-- " <> t
    appLine lline
appFullComment t = appLine "\n{-" >> appLine t >> appLine "-}"

-- name, type annotation, map itself
appTerm :: forall a . (Show a) => (Text -> Text) -> Text -> Text -> a -> TextT ()
appTerm pre mname mtype mmap = do
    appLine $ mname <> " :: " <> mtype
    appLine $ mname <> " = " <> pre (show mmap)
appMap :: forall a b . (Show a, Show b) => Text -> Text -> Map a b -> TextT ()
appMap = appTerm ("M." <>)
appList :: (Show a) => Text -> Text -> [a] -> TextT ()
appList = appTerm identity

genRule :: GrammarRule -> TextT ()
genRule GrammarRule{..} = pass

genParser :: GrammarDef -> Text
genParser g = snd $ flip runState "" $ do
    appText $ T.unlines $ take 4 $ T.lines codegenBase
    whenJust (gImports g) appLine
    appText $ T.unlines $ drop 4 $ T.lines codegenBase

    let grammarRules = convertGrammar $ gExprs g

    -- First/Follow
    let (firstS, followS) = setFirstFollow grammarRules
    appComment "First/follow"
    let firstComment =
            T.intercalate "\n" $
            flip map (M.assocs firstS) $ \(text,items) ->
            text <> " -> \"" <> T.intercalate "\", \"" (map prettyProdItem items) <> "\""
    appFullComment firstComment
    appNL >> appMap "firstGen" "Map Text [ProdItem]" firstS
    let followComment =
            T.intercalate "\n" $
            flip map (M.assocs followS) $ \(gr,items) ->
                let realItems = map (maybe "$" prettyProdItem) items
                in gr <> " -> \"" <> T.intercalate "\", \"" realItems <> "\""
    appFullComment followComment
    appNL >> appMap "followGen" "Map Text [Maybe ProdItem]" followS

    -- Tokens
    let tokens = gTokens g
    appComment "Tokens"
    appNL >> appList "tokensGen" "[TokenExp]" tokens

    -- Rules
    appComment "Rules"
    forM_ grammarRules genRule

----------------------------------------------------------------------------
-- Trash
----------------------------------------------------------------------------


kek :: IO ()
kek  = do
    (Right g) <- parseGrammar <$> TIO.readFile "resources/test2.g"
    TIO.writeFile "src/Haskll/Test.hs" $ genParser g
