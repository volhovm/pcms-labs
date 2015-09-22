{-# LANGUAGE UnicodeSyntax, ImplicitPrelude, OverloadedStrings #-}
module Main where

-- OK

import Prelude hiding (takeWhile, (**))
import System.IO
import Debug.Trace (trace)
import Data.Array
import Data.Attoparsec.ByteString.Char8
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BS
import qualified Data.Foldable as F
import Data.List (nub)
import Control.Monad
import Control.Applicative((<$>), (<*), (<|>))

fromLeft :: Either a b → b
fromLeft (Right b) = b

times :: Int → Parser a → Parser [a]
times 0 _ = return []
times 1 p = return <$> p
times n p = liftM2 (:) p (times (n - 1) p)

(**) = BS.append
--space = char ' '
space' = skipSpace <|> return ()
decimal' = decimal <* space'

processIO :: (Handle → Handle → IO()) → IO()
processIO handling = do
  input ← openFile "problem3.in" ReadMode
  output ← openFile "problem3.out" WriteMode
  handling input output
  hClose input
  hClose output

data Automaton = Automaton {
    isTerminal :: Int → Bool
  , aArray :: (Array Int [(Char, Int)])
  , aN :: Int
  , aM :: Int
  , aK :: Int
  }

instance Show Automaton where
  show (Automaton i a n m k) = show (map i [0..n-1]) ++ show a ++ " " ++
                               show n ++ " " ++ show m ++ " " ++ show k

nnc :: Parser (Int, (Char, Int))
nnc = liftM3 (\a b c → (a - 1, (c, b - 1))) decimal' decimal' (letter_ascii <* space')

inputPattern :: Parser (Automaton, Automaton)
inputPattern = do
  n ← decimal'
  m ← decimal'
  k ← decimal'
  accepting ← S.fromList <$> map (\x → x-1) <$> k `times` decimal'
  rawstates ← m `times` nnc
  let states = accumArray (\c a -> a:c) [] (0, n-1) rawstates
  let backStates = accumArray (\c a -> a:c) [] (0, n-1) $ map (\(a, (b, c)) → (c, (b, a))) rawstates
  return $ (Automaton (\x → S.member x accepting) states n m k,
            Automaton (\x → S.member x accepting) backStates n m k)

reachable :: Automaton → S.Set Int → S.Set Int → S.Set Int
reachable _ visited current | S.null current = visited
reachable aut visited current =
  let states = S.filter (not . flip S.member visited) $
               S.fromList $ F.concatMap (\curr → map snd $ (aArray aut) ! curr) current in
  let newvisited = visited `S.union` current in
  reachable aut newvisited states

backPropagate :: Automaton → S.Set Int → S.Set Int → Int → Maybe Integer
backPropagate _ valid used current | not (S.member current valid) = Just 0
backPropagate _ _ used current     | S.member current used = Nothing
backPropagate aut valid used current                       =
  let states = map (\(a, b) → b) $ (aArray aut) ! current in
  let newused = S.insert current used in
  let recursive = foldl (+) 0 <$> (mapM (backPropagate aut valid newused) states) in
  if (current == 0) then (+1) <$> recursive else recursive

main :: IO ()
main = processIO $ \input output → do
  pre ← (parseOnly inputPattern <$> BS.hGetContents input)
  let (aut, aut') = fromLeft $ pre
  let reachedAll = reachable aut S.empty $ S.singleton 0
  let reachedTerminal = S.filter (isTerminal aut) reachedAll
  let result =
        case F.foldlM (\acm end -> (+ acm) <$>
                                   (backPropagate aut' reachedAll S.empty end)) 0 reachedTerminal of
                 Nothing → "-1"
                 Just n  → show $ n `mod` (1000000007 :: Integer)

  hPutStrLn output result
  hPutStrLn stderr result
