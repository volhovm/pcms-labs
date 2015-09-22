{-# LANGUAGE UnicodeSyntax, ImplicitPrelude, OverloadedStrings #-}
module Main where

-- TL32, 100x100 matrix

import Prelude hiding (takeWhile, (**))
import System.IO
import Debug.Trace (trace)
import qualified Data.ByteString.Char8 as BS
import Data.Array.IArray
import Data.Array.Unboxed
import Data.Array.Base
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

decimal'' :: Parser Int
decimal'' = decimal <* space'

processIO :: (Handle → Handle → IO()) → IO()
processIO handling = do
  input ← openFile "problem4.in" ReadMode
  output ← openFile "problem4.out" WriteMode
  handling input output
  hClose input
  hClose output

data Pair = T !Int16 !Int16
            deriving (Eq, Ord, Ix)

nnc :: Parser (Pair, Int64)
nnc = liftM3 (\a b _ → (T (a - 1) (b - 1), 1)) decimal' decimal' (letter_ascii <* space')

inputPattern :: Parser (Int, [Int16], UArray Pair Int64)
inputPattern = do
  n ← decimal'
  m ← decimal'
  k ← decimal'
  l ← decimal''
  accepting ← map (\x → x-1) <$> k `times` decimal'
  states ← accumArray (+) 0 (T 0 0, T (n-1) (n-1)) <$> m `times` nnc
  return $ (l, accepting, states)

-- just for lulz
--sumA :: Array (Int, Int) Integer → Array (Int, Int) Integer → Array (Int, Int) Integer
--sumA arr1 arr2 | (bounds arr1) /= (bounds arr2) = undefined
--sumA arr1 arr2 = array (bounds arr1) [((i, j), s) |
--                                      i ← [0..n], j ← [0..n],
--                                      let s' = arr1 ! (i, j) + arr2 ! (i, j),
--                                      let s = s' `mod` 1000000007]
--                 where ((0, 0), (n, _)) = bounds arr1

expA :: UArray Pair Int64 → Int → UArray Pair Int64
--expA arr _ | let ((0,0), (n, m)) = bounds arr in n /= m = undefined
expA arr 1 = arr
expA arr q | odd q  = expA arr (q - 1) `mulA` arr
expA arr q | even q = arr' `mulA` arr'
  where arr' = arr `expA` (q `div` 2)

mulA :: UArray Pair Int64 → UArray Pair Int64 → UArray Pair Int64
--mulA arr1 arr2 | (bounds arr1) /= (bounds arr2) = undefined
mulA arr1 arr2 = newarr
  where (T _ _, T n _) = bounds arr1
        newarr = array (bounds arr1)
                 [(T i j, e) |
                  i ← [0..n],
                  j ← [0..n],
                  let e' = foldl1 (+) $ zipWith (*)
                           [a | u ← [0..n], let a = arr1 ! T i u]
                           [b | u ← [0..n], let b = arr2 ! T u j],
                        let e = e' `mod` 1000000007]

main :: IO ()
main = processIO $ \input output → do
  pre ← (parseOnly inputPattern <$> BS.hGetContents input)
  let (l, term, arr) = fromLeft $ pre
  let resarr = expA arr l
  --let result = foldl (\s terminal → (s + resarr ! (0, terminal)) `mod` 1000000007) 0 term
  hPutStrLn output $ show "lol"
