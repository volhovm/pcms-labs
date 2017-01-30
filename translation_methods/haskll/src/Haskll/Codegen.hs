{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

-- | Code generation.

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

import           Haskll.FirstFollow       (FirstSet, FollowSet, setFirstFollow)
import           Haskll.Grammar           (convertGrammar)
import           Haskll.Syntax.Expression (GrammarDef (..))
import           Haskll.Syntax.Parser     (parseGrammar)
import           Haskll.Types             (GrammarRule (..), ProdItem (..), bindVar,
                                           pName, prettyGrammarRule, prettyProdItem)

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
    appLine lline >> appNL
appFullComment t = appLine "\n{-" >> appLine t >> appLine "-}\n"
appLineIndent :: Int -> Text -> TextT ()
appLineIndent n t = appText (T.replicate n " ") >> appText t >> appText "\n"
-- name, type annotation, map itself
appTerm :: forall a . (Show a) => (Text -> Text) -> Text -> Text -> a -> TextT ()
appTerm pre mname mtype mmap = do
    appLine $ mname <> " :: " <> mtype
    appLine $ mname <> " = " <> pre (show mmap)
-- appMap :: forall a b . (Show a, Show b) => Text -> Text -> Map a b -> TextT ()
-- appMap = appTerm ("M." <>)
appList :: (Show a) => Text -> Text -> [a] -> TextT ()
appList = appTerm identity

genRule :: GrammarRule -> FirstSet -> FollowSet -> TextT ()
genRule GrammarRule{..} firstS followS = do
    appLine $ ruleName <> " :: HParser AST"
    appLine $ ruleName <> " = peekToken >>= \\case"
    forM_ grProds genProd
    genEpsFollow
    genErrorCase
  where
    ruleName = "parse"<>grName
    formList l = "[" <> T.intercalate "," (map (\e -> "\"" <> e ^. pName <> "\"") l) <> "]"
    genProd t@(x:_) = case x of
        (ProdTerminal pn _) -> do
            appLine $ "    Just Token{..} | tokenName == \"" <> pn <> "\" -> do"
            genConsume t
        (ProdNonterminal pn _ _)
            | not . null . filter (/= ProdEpsilon) $ firstS ! pn -> do
                let firstCur = filter (/= ProdEpsilon) $ firstS ! pn
                appLine $
                    "    Just Token{..} | tokenName `elem` " <>
                    formList firstCur <> " -> do"
                genConsume t
        _ -> pass
    genProd _ = panic "genRule#genProd empty list impossible"
    genEpsFollow
        | ProdEpsilon `elem` (firstS ! grName) = do
            let followCur = followS ! grName
                followNothing = any isNothing followCur
                followJust = any isJust followCur
                retVal = "pure $ ASTNode \"" <> grName <> "\" [ ASTLeafEps ]"
            when followJust $ do
                appLine $
                    "    Just Token{..} | tokenName `elem` " <>
                    formList (catMaybes followCur) <> " -> " <> retVal
            when followNothing $ appLine $ "    Nothing -> " <> retVal
        | otherwise = pass
    appLine8 = appLineIndent 8
    genErrorCase =
        appLine $ "    other -> panic $ \"" <> ruleName <>
        ": encountered unknow symbol: \" <> show other"
    genConsume items = do
        forM_ (items `zip` [0..]) $ uncurry consumeItem
        appLine8 $ "pure $ ASTNode \"" <> grName <> "\" [" <>
            T.intercalate "," (map (\i -> "retNode"<> show i) [0..length items - 1]) <> "]"
    consumeItem :: ProdItem -> Int -> TextT ()
    consumeItem (ProdTerminal pn _) ix = do
        appLine8 $ "retNode" <> show ix <> " <- consumeToken \"" <> pn <> "\""
    consumeItem (ProdNonterminal pn _ _) ix = do
        appLine8 $ "retNode" <> show ix <> " <- parse"<> pn
    consumeItem _ _                    = pass


genParser :: GrammarDef -> Text
genParser g = snd $ flip runState "" $ do
    appText $ T.unlines $ take 4 $ T.lines codegenBase
    whenJust (gImports g) appLine
    appText $ T.unlines $ drop 4 $ T.lines codegenBase

    let grammarRules = convertGrammar $ gExprs g

    appComment "Grammar"
    appFullComment $ T.intercalate "\n" $ map prettyGrammarRule grammarRules

    -- First/Follow
    let (firstS, followS) = setFirstFollow grammarRules
    appComment "First/follow"
    let firstComment =
            T.intercalate "\n" $
            flip map (M.assocs firstS) $ \(text,items) ->
            text <> " -> \"" <> T.intercalate "\", \"" (map prettyProdItem items) <> "\""
    appFullComment firstComment
    -- appMap "firstGen" "Map Text [ProdItem]" firstS
    let followComment =
            T.intercalate "\n" $
            flip map (M.assocs followS) $ \(gr,items) ->
                let realItems = map (maybe "$" prettyProdItem) items
                in gr <> " -> \"" <> T.intercalate "\", \"" realItems <> "\""
    appFullComment followComment
    -- appMap "followGen" "Map Text [Maybe ProdItem]" followS

    -- Tokens
    let tokens = gTokens g
    appComment "Tokens"
    appList "tokensGen" "[TokenExp]" tokens

    -- Rules
    appComment "Rules"
    forM_ grammarRules (\gl -> genRule gl firstS followS >> appNL)

    -- Bootstrapping
    appComment "Bootstrapping"
    appText $ "execParser :: FilePath -> IO AST\n\
              \execParser fp = do\n\
              \    contents <- TIO.readFile fp\n\
              \    let tokens =\n\
              \            either (panic \"Couldn't tokenize\") identity $\n\
              \            tokenize tokensGen (T.unpack contents)\n\
              \    pure $ evalHParser (HaskllState tokens) parsestart\n"

----------------------------------------------------------------------------
-- Trash
----------------------------------------------------------------------------


kek :: IO ()
kek  = do
    (Right g) <- parseGrammar <$> TIO.readFile "resources/test3.g"
    TIO.writeFile "src/Haskll/Test.hs" $ genParser g
