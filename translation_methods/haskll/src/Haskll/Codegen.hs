{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

-- | Code generation.

module Haskll.Codegen
       ( genParser
       , kek
       ) where

import           Control.Lens             ((%=))
import           Data.FileEmbed           (embedStringFile, makeRelativeToProject)
import           Data.Map                 ((!))
import qualified Data.Map                 as M
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Universum

import           Haskll.FirstFollow       (FirstSet, FollowSet, setFirstFollow)
import           Haskll.Grammar           (convertGrammar)
import           Haskll.Syntax.Expression (GrammarDef (..))
import           Haskll.Syntax.Parser     (parseGrammar)
import           Haskll.Types             (GrammarRule (..), ProdItem (..), pName,
                                           prettyGrammarRule, prettyProdItem)

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
    let extraArgTypes = map fst grReceivingAttrs
    let extraRetArgTypes = "(" <> T.intercalate "," (map fst grGeneratingAttrs) <> ")"
    appLine $ ruleName <> " :: " <>
        (T.concat $ map (<> " -> ") extraArgTypes) <>
        "HParser (AST, " <> extraRetArgTypes <> ")"
    let extraArgNames = map snd grReceivingAttrs
    appLine $ ruleName <> " " <> (T.intercalate " " extraArgNames) <>
        " = peekToken >>= \\case"
    forM_ grProds genProd
    genEpsFollow
    genErrorCase
  where
    ruleName = "parse"<>grName
    appLine4 = appLineIndent 4
    appLine8 = appLineIndent 8
    formList l =
        "[" <> T.intercalate "," (map (\e -> "\"" <> e ^. pName <> "\"") l) <> "]"
    genProd t@(x:_) = case x of
        (ProdTerminal pn _) -> do
            appLine4 $ "Just matchToken | tokenName matchToken == \"" <> pn <> "\" -> do"
            genConsume t
        (ProdNonterminal pn _ _)
            | not . null . filter (/= ProdEpsilon) $ firstS ! pn -> do
                let firstCur = filter (/= ProdEpsilon) $ firstS ! pn
                appLine4 $
                    "Just matchToken | tokenName matchToken `elem` " <>
                    formList firstCur <> " -> do"
                genConsume t
        _ -> pass
    genProd _ = panic "genRule#genProd empty list impossible"
    isEpsGenerating ProdEpsilon = True
    isEpsGenerating (ProdNonterminal name _ _) =
        fromMaybe False $ (ProdEpsilon `elem`) <$> M.lookup name firstS
    isEpsGenerating _ = False
    getEpsRule = fromMaybe (panic "The grammar is not LL1!") $
        find (\(x:_) -> isEpsGenerating x) grProds
    genEpsFollow
        | ProdEpsilon `elem` (firstS ! grName) = do
            let followCur = followS ! grName
                followNothing = any isNothing followCur
                followJust = any isJust followCur
            when followJust $ do
                appLine4 $
                    "Just matchToken | tokenName matchToken `elem` " <>
                    formList (catMaybes followCur) <> " -> do"
                genConsume getEpsRule
            when followNothing $ do
                appLine $ "    Nothing -> do"
                genConsume getEpsRule
        | otherwise = pass
    genErrorCase =
        appLine4 $ "other -> panic $ \"" <> ruleName <>
        ": encountered unknow symbol: \" <> show other"
    withNum :: [ProdItem] -> [(ProdItem,Maybe Int)]
    withNum = reverse . fst . foldl withNumFoo ([], 0::Int)
    withNumFoo (curList, i) p@(ProdCode _) = ((p,Nothing):curList, i)
    withNumFoo (curList, i) p@ProdEpsilon  = ((p,Nothing):curList, i)
    withNumFoo (curList, i) p              = ((p, Just i):curList, i+1)
    genConsume items = do
        let itemsNumbered = withNum items
            totalNumbers = length $ catMaybes $ map snd itemsNumbered
        forM_ itemsNumbered $ uncurry consumeItem
        let astNode = "ASTNode \"" <> grName <> "\" [" <>
                T.intercalate ","
                (map (\i -> "retNode" <> show i) [0..totalNumbers - 1]) <> "]"
            retVars = T.intercalate "," $ map snd grGeneratingAttrs
        appLine8 $ "pure $ (" <> astNode <> ", (" <> retVars <> "))"
    consumeItem :: ProdItem -> Maybe Int -> TextT ()
    consumeItem (ProdTerminal pn _) (Just ix) = do
        let smallPn = "token" <> pn
        appLine8 $ smallPn <> " <- consumeToken \"" <> pn <> "\""
        appLine8 $ "let retNode" <> show ix <> " = ASTLeaf " <> smallPn
    consumeItem (ProdNonterminal pn pargs var) (Just ix) = do
        let varName = fromMaybe pn var
        appLine8 $ "(retNode" <> show ix <> ", " <> varName <>
            ") <- parse"<> pn <> maybe "" (" "<>) pargs
    consumeItem (ProdCode code) _  = forM_ (map T.strip $ T.lines code) appLine8
    consumeItem _ _                    = pass


genParser :: GrammarDef -> Text
genParser g = snd $ flip runState "" $ do
    -- Imports and base
    appText $ T.unlines $ takeWhile (not . T.isPrefixOf "import") $ T.lines codegenBase
    whenJust (gImports g) appLine
    appText $ T.unlines $ dropWhile (not . T.isPrefixOf "import") $ T.lines codegenBase

    -- Members
    whenJust (gMembers g) $ \x -> appComment "Members" >> appLine x

    -- Grammar
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
              \    let (ast,res) = evalHParser (HaskllState tokens) parsestart\n\
              \    print res\n\
              \    pure ast\n"

----------------------------------------------------------------------------
-- Trash
----------------------------------------------------------------------------


kek :: IO ()
kek  = do
    g <- parseGrammar <$> TIO.readFile "resources/test4.g"
    TIO.writeFile "src/Haskll/Test.hs" $ genParser $ either onLeft identity g
  where
    onLeft x = panic $ "Could not parse: " <> x
