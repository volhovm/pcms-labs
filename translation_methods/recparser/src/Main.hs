{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad             (void, when)
import           Control.Monad.Except      (runExceptT)
import           Data.Either.Combinators   (fromLeft', fromRight', isLeft)
import           Data.Tree                 (Tree (..))
import           Data.Tree.Pretty          (drawVerticalTree)
import           Diagrams.Backend.SVG      (SVG (..), renderSVG)
import           Diagrams.Prelude          (( # ), (&), (.~), (<>), (~~))
import qualified Diagrams.Prelude          as D
import qualified Diagrams.TwoD.Layout.Tree as D
import           System.IO                 (Handle, IOMode (ReadMode), openFile)
import qualified Turtle                    as T

import           Lexer
import           Parser

parseGrammar :: Handle -> IO PA
parseGrammar handle = do
    tokens <- runExceptT (lexicalAnalyzer handle)
    when (isLeft tokens) $ error $ "Lexer failed: " ++ show (fromLeft' tokens)
    putStrLn $ "Tokens: " ++ show tokens
    let parsed = runParser $ fromRight' tokens
    when (isLeft parsed) $ error $ "Parser failed: " ++ fromLeft' parsed
    return $ fromRight' parsed

prettyPrintTree :: Tree String -> String
prettyPrintTree = drawVerticalTree

class TreeLike a where
    toTree :: a -> Tree String

instance TreeLike PA where
    toTree (PANum i) = Node "A" [Node (show i) []]
    toTree (PAPrefix i pb) = Node "A" [Node (show i) [], toTree pb]

instance TreeLike PB where
    toTree ((:+) pa) = Node "B" [toTree pa, Node "+" []]
    toTree ((:-) pa) = Node "B" [toTree pa, Node "-" []]
    toTree ((:*) pa) = Node "B" [toTree pa, Node "*" []]
    toTree ((:++) pa pb) = Node "B" [toTree pa, Node "+" [], toTree pb]
    toTree ((:--) pa pb) = Node "B" [toTree pa, Node "-" [], toTree pb]
    toTree ((:**) pa pb) = Node "B" [toTree pa, Node "*" [], toTree pb]

instance Show PA where
    show t = prettyPrintTree $ toTree t

instance Show PB where
    show t = prettyPrintTree $ toTree t

getTree :: IO (Tree String)
getTree = do
  test0 <- openFile "test5.in" ReadMode
  toTree <$> parseGrammar test0

cast :: (Integral a) => a -> Double
cast = fromInteger . toInteger

genTreeDiagram :: Tree String -> Int -> D.QDiagram SVG D.V2 Double D.Any
genTreeDiagram tree maxLength =
    D.renderTree
        (\(a :: String) ->
              D.text a <> D.roundedRect (cast $ length a) 1.2 0.1 # D.fc D.grey #
              D.lw 2)
        (~~)
        (D.symmLayout'
             (D.with & D.slHSep .~ max 1.5 (cast maxLength) & D.slVSep .~
              1.5)
             tree) #
    D.centerXY #
    D.pad 1.1


main :: IO ()
main = do
    tree <- getTree
    putStrLn $ prettyPrintTree tree
    let maxStringLength = maximum $ fmap length tree
    print maxStringLength
    renderSVG
        "diagram.svg"
        (D.mkWidth (min (500 * max 1 (cast maxStringLength / 3)) 2000))
        (genTreeDiagram tree maxStringLength)
    void $ T.shell "xdg-open ./diagram.svg" T.empty
