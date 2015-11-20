{-# LANGUAGE UnicodeSyntax, ImplicitPrelude, OverloadedStrings #-}
module Main where

-- NOT FINISHED

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
import qualified Data.Map as M
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
  input ← openFile "problem5.in" ReadMode
  output ← openFile "problem5.out" WriteMode
  handling input output
  hClose input
  hClose output

data Pair1 = T1 !Int16 !Char
            deriving (Eq, Ord, Ix)

data Pair2 = T2 !Int16 !Int16
            deriving (Eq, Ord, Ix)

instance Show Pair1 where
  show (T1 a b) = "<"++ show a ++ "," ++ show b ++ ">"

instance Show Pair2 where
  show (T2 a b) = "<"++ show a ++ "," ++ show b ++ ">"

data Automaton = Automaton {
    aTerminal :: [Int16]
  , aArray :: Array Pair1 [Int16]
  }

nnc :: Parser (Pair1, [Int16])
nnc = liftM3 (\a b c → (T1 (a - 1) c,  [b - 1])) decimal' decimal' (letter_ascii <* space')

inputPattern :: Parser (Int16, Automaton)
inputPattern = do
  n ← decimal'
  m ← decimal'
  k ← decimal'
  l ← decimal'
  accepting ← map (\x → x-1) <$> k `times` decimal'
  states ← accumArray (\a s → s ++ a) [] (T1 0 'a', T1 (n - 1) 'z') <$> m `times` nnc
  return $ (l, Automaton accepting states)

dynamic :: UArray Pair2 Int64 → UArray Pair2 Int64 → S.Set Int16
           → Int16 → Int16 → UArray Pair2 Int64
dynamic _ dyn _ l maxl | l >= maxl = dyn
dynamic arr dyn current l maxl =
    let (_, (T2 upb _)) = bounds arr in
    let children :: [(Pair2, (Int16, Int64))]
        children = concat $ map (\n → [((T2 u (l+1)), (n, counter)) |
                          u ← [0..upb],
                          let counter = arr ! (T2 n u),
                          counter > 0]) $ S.elems current in
    let children0 :: S.Set Int16
        children0 = S.fromList $ map (\((T2 a _), _) → a) children in
    let newdyn = accum (\score (u, num) → let new = (score + ((dyn ! T2 u l) * num)) in
             new `mod` 1000000007)
             dyn children in
                            if null children
                            then dyn
                            else dynamic arr newdyn children0 (l+1) maxl

--
thomgen :: Automaton → S.Set Int16 → S.Set (S.Set Int16) → M.Map (S.Set Int, S.Set Int) Char
           → (UArray Pair2 Int64, [Int16])
thomgen aut _P _Qd _delta | S.null _P = (undefined, undefined)

thompson :: Automaton → (UArray Pair2 Int64, [Int16])
thompson aut = thomgen aut (S.singleton 0) S.empty M.empty

main :: IO ()
main = processIO $ \input output → do
  pre ← (parseOnly inputPattern <$> BS.hGetContents input)
  let (l, aut) = fromLeft $ pre
  let (arr, term) = thompson aut
  let (_, (T2 upb _)) = bounds arr
  let result = dynamic arr
               ((accumArray (\a _ → a) 0 (T2 0 0, T2 upb l) []) // [((T2 0 0), 1)])
               (S.singleton 0) 0 l
  let sm = (foldl (+) 0 $ map (\x → result ! (T2 x l)) term) `mod` 1000000007
  hPutStrLn output $ show sm
  hPutStrLn stderr $ show sm
