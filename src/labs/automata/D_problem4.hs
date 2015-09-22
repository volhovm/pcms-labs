{-# LANGUAGE UnicodeSyntax, ImplicitPrelude, OverloadedStrings #-}
module Main where

-- OK

import Prelude hiding (takeWhile, (**))
import System.IO
import Debug.Trace (trace)
import qualified Data.ByteString.Char8 as BS
import Data.Array.IArray
import Data.Array.Unboxed
import Data.Attoparsec.ByteString.Char8
import Data.Int
import qualified Data.Set as S
import qualified Data.Foldable as F
import Data.List (nub)
import Control.Monad
import Control.Applicative((<$>), (<*), (<|>))

fromLeft :: Either a b → b
fromLeft (Right b) = b

times :: Int16 → Parser a → Parser [a]
times 0 _ = return []
times 1 p = return <$> p
times n p = liftM2 (:) p (times (n - 1) p)

(**) = BS.append
--space = char ' '
space' = skipSpace <|> return ()

decimal' :: Parser Int16
decimal' = decimal <* space'

processIO :: (Handle → Handle → IO()) → IO()
processIO handling = do
  input ← openFile "problem4.in" ReadMode
  output ← openFile "problem4.out" WriteMode
  handling input output
  hClose input
  hClose output

data Pair = T Int16 Int16
            deriving (Eq, Ord, Ix)

instance Show Pair where
  show (T a b) = "<"++ show a ++ "," ++ show b ++ ">"

nnc :: Parser (Pair, Int64)
nnc = liftM3 (\a b _ → (T (a - 1) (b - 1), 1)) decimal' decimal' (letter_ascii <* space')

inputPattern :: Parser (Int16, [Int16], UArray Pair Int64)
inputPattern = do
  n ← decimal'
  m ← decimal'
  k ← decimal'
  l ← decimal'
  accepting ← map (\x → x-1) <$> k `times` decimal'
  states ← accumArray (+) 0 (T 0 0, T (n-1) (n-1)) <$> m `times` nnc
  return $ (l, accepting, states)

dynamic :: UArray Pair Int64 → UArray Pair Int64 → S.Set Int16 → Int16 → Int16 → UArray Pair Int64
dynamic _ dyn _ l maxl | l >= maxl = dyn
dynamic arr dyn current l maxl =
    let (_, (T upb _)) = bounds arr in
    let children :: [(Pair, (Int16, Int64))]
        children = concat $ map (\n → [((T u (l+1)), (n, counter)) |
                          u ← [0..upb],
                          let counter = arr ! (T n u),
                          counter > 0]) $ S.elems current in
    let children0 :: S.Set Int16
        children0 = S.fromList $ map (\((T a _), _) → a) children in
    let newdyn = accum (\score (u, num) → let new = (score + ((dyn ! T u l) * num)) in
             new `mod` 1000000007)
             dyn children in
                            if null children
                            then dyn
                            else dynamic arr newdyn children0 (l+1) maxl



main :: IO ()
main = processIO $ \input output → do
  pre ← (parseOnly inputPattern <$> BS.hGetContents input)
  let (l, term, arr) = fromLeft $ pre
  let (_, (T upb _)) = bounds arr
  let result = dynamic arr
               ((accumArray (\a _ → a) 0 (T 0 0, T upb l) []) // [((T 0 0), 1)])
               (S.singleton 0) 0 l
  let sm = (foldl (+) 0 $ map (\x → result ! (T x l)) term) `mod` 1000000007
  hPutStrLn output $ show sm
  hPutStrLn stderr $ show sm
