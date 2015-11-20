module Main where

-- By Dima Mukhutdinov

import Data.List
import System.IO
import System.Environment

data Edge = Edge Char AutTree
data AutTree = Nont { ind :: Int
                    , lt :: Edge
                    , rt :: Edge }
             | Term { ind :: Int }

genTree :: Int -> Int -> AutTree
genTree idx 0 = Term idx
genTree idx n = Nont idx
                (Edge 'a' (genTree (2*idx) (n - 1)))
                (Edge 'b' (genTree (2*idx + 1) (n - 1)))

getTerms (Term i) = [i]
getTerms (Nont _ (Edge _ l) (Edge _ r)) = getTerms l ++ getTerms r

showTree (Term i) = ""
showTree (Nont i (Edge a l) (Edge b r)) = unlines [ unwords [show i, show (ind l), [a]]
                                                  , unwords [show i, show (ind r), [b]]
                                                  ] ++ showTree l ++ showTree r

instance Show AutTree where
    show = showTree

treeSize (Term _) = 1
treeSize (Nont _ (Edge _ l) (Edge _ r)) = 1 + treeSize l + treeSize r

main :: IO ()
main = do
  (sn:_) <- getArgs
  let n = read sn
      tree = genTree 1 n
      terms = getTerms tree
      size = treeSize tree
  putStrLn $ unwords $ map show [size, size - 1, length terms]
  putStrLn $ unwords $ map show terms
  putStr $ show tree
