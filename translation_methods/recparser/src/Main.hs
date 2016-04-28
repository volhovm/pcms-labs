{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad             (void)
import qualified Data.ByteString.Char8     as BS
import           Data.Tree                 (Tree (..))
import           Data.Tree.Pretty          (drawVerticalTree)
import           Diagrams.Backend.SVG      (SVG (..), renderSVG)
import           Diagrams.Prelude          (( # ), (&), (.~), (<>), (~~))
import qualified Diagrams.Prelude          as D
import qualified Diagrams.TwoD.Layout.Tree as D
import qualified GHC.IO.Encoding           as E
import           System.IO                 (IOMode (ReadMode), openFile)
import qualified Turtle                    as T

import           Recparser


prettyPrintTree :: Tree String -> String
prettyPrintTree = drawVerticalTree

class TreeLike a where
    toTree :: a -> Tree String

instance TreeLike PA where
    toTree (PANum i) = Node "A" [Node (show i) []]
    toTree (PAPrefix i pb) = Node "A" [Node (show i) [], toTree pb]

instance TreeLike PB where
    toTree (PB pa pc) = Node "B" [toTree pa, toTree pc]

instance TreeLike PC where
    toTree ((:+) pd) = Node "C" [toTree pd, Node "+" []]
    toTree ((:-) pd) = Node "C" [toTree pd, Node "-" []]
    toTree ((:*) pd) = Node "C" [toTree pd, Node "*" []]

instance TreeLike PD where
    toTree PDε = Node "ε" []
    toTree (PDB pb) = toTree pb

instance Show PA where
    show t = prettyPrintTree $ toTree t

instance Show PB where
    show t = prettyPrintTree $ toTree t

getTree :: IO (Tree String)
getTree = do
  E.setLocaleEncoding E.utf8
  handle <- openFile "test4.in" ReadMode
  res <- parseGrammar <$> BS.hGetContents handle
  case res of
      Left str -> error str
      Right pa -> return $ toTree pa

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
    E.setLocaleEncoding E.utf8
    tree <- getTree
    putStrLn $ prettyPrintTree tree
    let maxStringLength = maximum $ fmap length tree
    putStrLn $ "Max string length: " ++ show maxStringLength
    renderSVG
        "diagram.svg"
        (D.mkWidth (min (500 * max 1 (cast maxStringLength / 3)) 2000))
        (genTreeDiagram tree maxStringLength)
    void $ T.shell "xdg-open ./diagram.svg" T.empty
